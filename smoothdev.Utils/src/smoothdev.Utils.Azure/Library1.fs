namespace smoothdev.Utils.Azure

open FSharp.Data
open FSharp.Management
open Hopac
type FS = FileSystem<const(__SOURCE_DIRECTORY__)>
type AzureSettings = XmlProvider<FS.``dummy.azure.settings.config``>
type AzurePublishSettings = XmlProvider<FS.``dummy.publishsettings``>

module internal Internals =
  open System.Security.Cryptography.X509Certificates

  let certFromBase64String = System.Convert.FromBase64String >> X509Certificate2

open Internals

type AzureCloudSettings = 
  private { subscription: AzurePublishSettings.Subscription; azureSettings: AzureSettings.AzureSettings }
  with
    
    static member Make subscription azureSettings = { subscription = subscription; azureSettings = azureSettings }
    
    member x.Region = x.azureSettings.CloudService.Provisionning.Region
    member x.SubscriptionId = string x.subscription.Id

    /// this is equal to https://management.core.windows.net/
    member x.ServiceManagementUrl = 
      if x.subscription.ServiceManagementUrl.EndsWith("/") 
      then x.subscription.ServiceManagementUrl
      else x.subscription.ServiceManagementUrl + "/"
    member x.ActiveDirectoryLogin = x.azureSettings.ActiveDirectory.Login
    member x.ActiveDirectoryPassword = x.azureSettings.ActiveDirectory.Password
    member x.ActiveDirectoryAuthenticationUrl = sprintf "https://login.windows.net/%s" (string x.azureSettings.ActiveDirectory.TenantId)
    member x.ActiveDirectoryClientId = string x.azureSettings.ActiveDirectory.ClientId
    member x.ActiveDirectoryCertificate = 
      x.azureSettings.ActiveDirectory.Certificate
      |> certFromBase64String

    member x.ManagementCertificate =
      x.subscription.ManagementCertificate
      |> certFromBase64String

module AzureSettingsLoader =
  let loadSettings (azureSettingsFilename: string) =
    try
      Some (AzureSettings.Load azureSettingsFilename)
    with 
    | _ -> None

  let GetAzureCloudSettings (azureSettingsFilename) =
    match loadSettings azureSettingsFilename with
    | None -> None
    | Some settings -> 
      let publishSettings = AzurePublishSettings.Load settings.Subscription.File
      let subscription =
        publishSettings.PublishProfile.Subscriptions
        |> Seq.tryFind (fun s -> s.Name = settings.Subscription.Name)
      match subscription with
      | None -> None
      | Some subscription -> Some (AzureCloudSettings.Make subscription settings) 

module Authentication =
  open Microsoft.IdentityModel.Clients.ActiveDirectory
  open Microsoft.Rest
  open Microsoft.Azure
  
  let authenticate (settings: AzureCloudSettings) =
    let authenticationContext = AuthenticationContext settings.ActiveDirectoryAuthenticationUrl
    let userCredential        = UserPasswordCredential (settings.ActiveDirectoryLogin, settings.ActiveDirectoryPassword)
    authenticationContext.AcquireTokenAsync (settings.ServiceManagementUrl, settings.ActiveDirectoryClientId, userCredential)

  let serviceCredentials settings = 
    job {
      let! adToken = authenticate settings
      return 
        StringTokenProvider((adToken: AuthenticationResult).AccessToken, adToken.AccessTokenType)
        |> TokenCredentials
    }
  
  let cloudCredentials settings = 
    job {
      let! adToken = authenticate settings
      return TokenCloudCredentials (settings.SubscriptionId, (adToken: AuthenticationResult).AccessToken)
    }
    
module ComputeManagement = 
  open Microsoft.Azure.Management.Compute
  open Authentication

  let getComputeManagementClient settings = 
    job {
      let! credentials = serviceCredentials settings
      return new ComputeManagementClient(credentials, SubscriptionId = settings.SubscriptionId)
    }

  type WastedCoreAndTotalMachineCount = { WastedCores: int; MachineCount : int}
  type VmDescription = private { 
    azureVm   : Models.VirtualMachineSize option
    coreCount : int
    ram       : int
  }
  with

    static member Create vm =
      { azureVm   = Some vm
        coreCount = vm.NumberOfCores.Value
        ram       = vm.MemoryInMB.Value }

    static member Create(coreCount, ram) =
      { azureVm   = None
        coreCount = coreCount
        ram       = ram }

    member x.Ram        = x.ram
    member x.CoreCount  = x.coreCount
    member x.Name       = x.azureVm |> function | None -> "" | Some vm -> vm.Name
    member x.RamPerCore = x.ram / x.coreCount

    member x.ComputeWastedCoreAndNecessaryMachineCount totalCoreCount =
      let wastedCores = totalCoreCount % x.coreCount
      let numberOfMachines = totalCoreCount / x.coreCount
      { WastedCores = wastedCores; MachineCount = numberOfMachines }

  let queryVmSizes settings = job {
    use! client = getComputeManagementClient settings
    return
      client.VirtualMachineSizes.List(settings.Region)
      |> Seq.map VmDescription.Create
  }

  let computeSizes vmSizes totalCoreCount minimumRamPerCore maximumWastedCoreRatio =
    
    let hasEnoughMemory (vm: VmDescription) = vm.RamPerCore >= minimumRamPerCore

    let possibleMachines = 
      vmSizes
      |> Seq.filter (hasEnoughMemory)
      |> Seq.map (fun m -> m.ComputeWastedCoreAndNecessaryMachineCount totalCoreCount, m)
      |> Seq.filter (fun (wasteResult, _) -> (float wasteResult.WastedCores) / (float totalCoreCount) <= maximumWastedCoreRatio)
      |> Seq.sortByDescending (fun (_,vm) -> vm.coreCount)

    possibleMachines

module SqlManagement =
  open Microsoft.Azure

  let getClient settings =
    job {
      let! credentials = Authentication.cloudCredentials settings
      return new Microsoft.Azure.Management.Sql.SqlManagementClient(credentials)
    }
