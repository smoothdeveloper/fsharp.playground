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
 
let getCsvLines (filePath: string) =
    File.ReadLines(filePath)
    |> (1 |> Seq.skip) // skip csv header line
 
type DataRecord = {Number:int; Data:byte []}

let convertStringToRecord (lineRecord: string) =
    let values = lineRecord.Split(',')
    let number = values |> Seq.head |> Int32.Parse
    let data =  values |> Seq.skip 1 |> Seq.map Byte.Parse |> Seq.toArray
    {Number=number; Data = data}


type ClassificationData = IDictionary<int,byte [][]>

let createProcessingDataSet (records: DataRecord seq) : ClassificationData =
    let getData (record: DataRecord) =
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

type NumberDistance = {Number: int; Distance: float}
type ClassifierResult = {Prediction: int; Distances: NumberDistance []}

let classifier (dataset:ClassificationData) (data: byte []) =
    let averages =
        dataset
        |> Seq.map (fun (kvp) -> (kvp.Key, kvp.Value)) // wish f# had a way to deconstruct KeyValuePair<K,V>
        |> Seq.map (fun (k,values) ->
            let averagedDistance = 
                values 
                |> Seq.map (fun v -> computeDataDistance v data)
                |> Seq.average
            (k, averagedDistance)
        )
        |> Seq.sortBy (fun (k,d) -> d)
        |> Seq.map (fun (k,d) -> {Number = k; Distance = d})
    {Prediction = averages |> Seq.head |> (fun nd -> nd.Number); Distances = Seq.toArray averages}

let allRecords =
    getCsvLines trainingSampleCsvPath
    |> Seq.map convertStringToRecord
    |> Seq.toArray

let trainingDataset = createProcessingDataSet allRecords

let oneRecord = { Data= trainingDataset.[1].[100]; Number= -11}

//> classifier trainingDataset oneRecord

let records = getCsvLines trainingSampleCsvPath
                |> Seq.map convertStringToRecord
                |> Seq.map (fun r -> r.Number)
                |> Seq.distinct
                |> Seq.iter (fun v -> Console.WriteLine(v))
(* // compute distance samples
let d1 = [|1 |> byte; 2 |> byte|]
let d2 = [|0 |> byte; 5 |> byte|]
computeDataDistance d1  d2
*) //

let rateClassifier (data: DataRecord seq) (predictor: (byte[] -> int)) (dataset: ClassificationData) =
    let count = Seq.length data |> float
    let predictions = 
        data
        |> Seq.map (fun d -> 
            let prediction = predictor d.Data
            (d.Number = prediction, prediction)
        )
    let correctAnswers = predictions |> Seq.filter (fun (isMatch, _) -> isMatch) |> Seq.length |> float
    correctAnswers / count

rateClassifier (allRecords |> Seq.take 100) (fun r -> (classifier trainingDataset r).Prediction) trainingDataset

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
    let image = new Bitmap(n, rows)
    let twoDimensionData = DigitRecognizer.batch data n |> Seq.zip [0..(rows)]
    for (y, rowOfPixels) in twoDimensionData do
        let pixels = rowOfPixels |> Seq.zip [|0..n|]
        pixels |> Seq.iter (fun (x, v) -> image.SetPixel(x, y, Color.FromArgb(v |> int, v |> int, v |> int)))
    showImage image

displayData [|(0|> byte)..(255|> byte)|] 10
displayData oneRecord.Data 28

trainingDataset.[9] |> Seq.take 10 |> Seq.map (fun d -> displayData d 28)

// visualise distance in a naive way
let showAbsoluteDistance (x: byte[]) (y: byte[]) =
    let distance = 
        Seq.zip x y
        |> Seq.map (fun (a, b) -> (a|>int) - (b|>int))
        |> Seq.map Math.Abs
        |> Seq.map byte
        |> Seq.toArray
    displayData distance 28
//> showAbsoluteDistance trainingDataset.[9].[0] trainingDataset.[9].[50]
