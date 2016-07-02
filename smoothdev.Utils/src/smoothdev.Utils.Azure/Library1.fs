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


module ActiveDirectory =
  open Microsoft.IdentityModel.Clients.ActiveDirectory
  
  let authenticate (settings: AzureCloudSettings) =
    let authenticationContext = AuthenticationContext settings.ActiveDirectoryAuthenticationUrl
    let userCredential        = UserPasswordCredential (settings.ActiveDirectoryLogin, settings.ActiveDirectoryPassword)
    authenticationContext.AcquireTokenAsync (settings.ServiceManagementUrl, settings.ActiveDirectoryClientId, userCredential)
  
module SqlManagement =
  open Microsoft.Azure

  let authenticate settings = 
    job {
      let! adToken = ActiveDirectory.authenticate settings
      return TokenCloudCredentials (settings.SubscriptionId, adToken.AccessToken)
    }

  let getClient settings =
    job {
      let! credentials = authenticate settings
      return new Microsoft.Azure.Management.Sql.SqlManagementClient(credentials)
    }

type Class1() = 
    member this.X = "F#"
