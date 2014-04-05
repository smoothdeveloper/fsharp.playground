module DigitRecognizer

    open System.Collections.Generic
    let batch<'a> (data: 'a seq ) (n:int) =
        let count = Seq.length data
        let totalRows = count / n
        let nextBatch = ref (new List<'a>(n))
        seq {
            for v in data do
                (!nextBatch).Add(v)
                if (!nextBatch).Count = n then
                    yield !nextBatch
                    nextBatch := new List<'a>(n)
            if (!nextBatch).Count > 0 then
                yield !nextBatch
        }

    // sample: batch [|1..100|] 10