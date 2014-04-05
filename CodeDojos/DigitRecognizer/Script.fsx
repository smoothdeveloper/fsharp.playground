// Learn more about F# at http://fsharp.net. See the 'F# Tutorial' project
// for more guidance on F# programming.
#time
#load "Library.fs"
open DigitRecognizer
open System.IO
open System
open System.Collections.Generic
// Define your library scripting code here
 
let storageFolder = @"C:\home\work\codedojos\Dojo-Digits-Recognizer\Dojo"
 
let trainingSampleCsvPath = Path.Combine(storageFolder, "trainingsample.csv")
let validationCsvPath = Path.Combine(storageFolder, "validationsample.csv")
 
let parseCsv (filePath: string) =
    File.ReadLines(filePath)
    |> (1 |> Seq.skip)
    //|> Seq.take 100000000
 
type validationRecord = {Number:int; Data:byte []}

let convertStringToRecord (lineRecord: string) =
    let values = lineRecord.Split(',')
    let number = values |> Seq.head |> Int32.Parse
    let data =  values |> Seq.skip 1 |> Seq.map Byte.Parse |> Seq.toArray
    {Number=number; Data = data}
//parseCsv trainingSampleCsvPath |>Seq.skip 17 |> Seq.map convertStringToRecord |>


type ClassificationData = IDictionary<int,byte [][]>

let createProcessingDataSet (records:validationRecord seq) : ClassificationData =
    let getData (record:validationRecord) =
        record.Data
    records
    |> Seq.groupBy (fun (n) -> n.Number)
    |> Seq.map (fun (key,values) -> (key, values |> (Seq.map getData) |> Seq.toArray))
    |> dict

let computeDataDistance (x:byte []) (y:byte []) =
    Seq.zip x y
    |> Seq.map (fun (a, b) -> (a|>float) - (b|>float))
    |> Seq.map (fun w -> w ** 2.0)
    |> Seq.sum
    |> System.Math.Sqrt

let classifier (dataset:ClassificationData ) (record: validationRecord) =
    dataset
    |> Seq.map (fun (kvp) -> (kvp.Key, kvp.Value))
    |> Seq.map (fun (k,values) ->
        let averagedDistance = values |> Seq.map (fun v -> computeDataDistance v record.Data) |> Seq.average
        (k, averagedDistance)
    )

let trainingDataset =
    parseCsv trainingSampleCsvPath
    |> Seq.map convertStringToRecord
    |> createProcessingDataSet
trainingDataset.Keys |> Seq.sort |> Seq.map  (fun c -> Console.WriteLine(c))
let oneRecord = { Data= trainingDataset.[5].[4]; Number= -11}

classifier trainingDataset oneRecord |> Seq.map (fun c -> Console.WriteLine(c))

let records = parseCsv trainingSampleCsvPath
                |> Seq.map convertStringToRecord
                |> Seq.map (fun r -> r.Number)
                |> Seq.distinct
                |> Seq.iter (fun v -> Console.WriteLine(v))
(* // compute distance samples
let d1 = [|1 |> byte; 2 |> byte|]
let d2 = [|0 |> byte; 5 |> byte|]
computeDataDistance d1  d2
*) //

open DigitRecognizer
DigitRecognizer.batch [|1..100|] 10

open System.Drawing
open System.Windows.Forms
let showImage (image: Image) =
    let form = new Form()
    let imageDisplay = new PictureBox(Dock=DockStyle.Fill)
    imageDisplay.Image <- image
    form.Controls.Add(imageDisplay)
    form.Show()

let displayData (data: byte[]) (n: int) =
    let rows = data.Length / n + (if data.Length % n > 0 then 1 else 0)
    let image = new Bitmap(rows, n)
    let twoDimensionData = DigitRecognizer.batch data n |> Seq.zip [0..(rows)]
    for (x, rowOfPixels) in twoDimensionData do
        let pixels = rowOfPixels |> Seq.zip [|0..n|]
        pixels |> Seq.iter (fun (y, v) -> image.SetPixel(x, y, Color.FromArgb(v |> int, v |> int, v |> int)))
    showImage image

displayData [|(0|> byte)..(255|> byte)|] 10
displayData oneRecord.Data 28

