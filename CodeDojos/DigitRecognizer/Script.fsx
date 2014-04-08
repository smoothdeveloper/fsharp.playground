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

let classifier1 (dataset:ClassificationData) (data: byte []) =
    (*
    this classifier is comparing distance against every sample and average the distances
    then take the shortest averaged distance as the prediction
    this is very slow, and not very accurate
    *)
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

let averageData (data: byte[] seq) =
    let count = Seq.length data
    let firstArrayLength = (Seq.head data).Length
    let zero = Array.init<uint64> firstArrayLength (fun _ -> 0 |> uint64)
    data
    |> Seq.map (fun d -> d |> Array.map uint64)
    |> Seq.fold (fun prev cur -> Array.map2 (+) prev cur) zero
    |> Seq.map (float)
    |> Seq.map (fun v -> v / (count |> float))
    |> Seq.map (byte)
    |> Seq.toArray

let mixedTrainingDatasetToSingleImage = 
    trainingDataset 
    |> Seq.map (fun kvp -> (kvp.Key, kvp.Value))
    |> Seq.map (fun (k,v) -> (k, averageData v))
    |> dict

let classifier2 (data: byte[]) =
    (*
    this classifier works on a simplified training dataset where all images are averaged together 
    (for each of 10 digits, only a single smoothed image to match on)
    far quicker than classifier1 (compute only 10 distances rather than computing it for every sample) 
    and also far more accurate in results
    *)
    let predictions = 
        mixedTrainingDatasetToSingleImage
        |> Seq.map (fun kvp -> (kvp.Key, computeDataDistance kvp.Value data))
        |> Seq.sortBy (fun (k,d) -> d)
    {Prediction = (predictions |> Seq.head |> fst) ; Distances = predictions |> Seq.map (fun (k,d) -> {Number = k; Distance = d}) |> Seq.toArray }

let oneRecord = { Data= trainingDataset.[1].[100]; Number= -11}

let records = getCsvLines trainingSampleCsvPath
                |> Seq.map convertStringToRecord
                |> Seq.map (fun r -> r.Number)
                |> Seq.distinct
                |> Seq.iter (fun v -> Console.WriteLine(v))

let rateClassifier (data: DataRecord seq) (predictor: (byte[] -> int)) (dataset: ClassificationData) =
    let count = Seq.length data |> float
    let counter = ref 0
    let matched = ref 0
    let mismatched = ref 0
    let predictions = 
        data
        |> Seq.map (fun d -> 
            let prediction = predictor d.Data
            counter := (!counter + 1) 
            if prediction = d.Number then matched := (!matched + 1)
            else mismatched := (!mismatched + 1)
            Console.WriteLine("processed {0} (matched: {1}, mismatched: {2})", !counter, !matched, !mismatched)
            (d.Number = prediction, prediction)
        )
    let correctAnswers = predictions |> Seq.filter (fun (isMatch, _) -> isMatch) |> Seq.length |> float
    correctAnswers / count

rateClassifier (allRecords |> Array.toSeq) (fun r -> (classifier1 trainingDataset r).Prediction) trainingDataset
rateClassifier (allRecords |> Array.toSeq) (fun r -> (classifier2 r).Prediction) trainingDataset

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

let displayData (n: int)  (data: byte[]) =
    let rows = data.Length / n + (if data.Length % n > 0 then 1 else 0)
    let image = new Bitmap(n, rows)
    let twoDimensionData = DigitRecognizer.batch data n |> Seq.zip [0..(rows)]
    for (y, rowOfPixels) in twoDimensionData do
        let pixels = rowOfPixels |> Seq.zip [|0..n|]
        pixels |> Seq.iter (fun (x, v) -> image.SetPixel(x, y, Color.FromArgb(v |> int, v |> int, v |> int)))
    showImage image

displayData 10 [|(0|> byte)..(255|> byte)|]
displayData 28 oneRecord.Data
averageData trainingDataset.[9] |> displayData 28

trainingDataset.[9] |> Seq.take 10 |> Seq.map (fun d -> displayData 28 d)

// visualise distance in a naive way
let showAbsoluteDistance (x: byte[]) (y: byte[]) =
    let distance = 
        Seq.zip x y
        |> Seq.map (fun (a, b) -> (a|>int) - (b|>int))
        |> Seq.map Math.Abs
        |> Seq.map byte
        |> Seq.toArray
    displayData 28 distance
//> showAbsoluteDistance trainingDataset.[9].[0] trainingDataset.[9].[50]
