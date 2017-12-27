# patents

I'm learning how to parse big CSV files in Haskell. This is my [third attempt](https://github.com/dmvianna/framesy). I'll be trying things (hopefully) that are almost directly translatable to my work, as parsing addresses out of free text. Good luck to me!

## The dataset
The data we'll be analysing are the [public records of Australian patents](https://ipaustralia.gov.au/about-us/economics-ip/ip-government-open-data). You can get the CSV files from data.gov.au. No, I will not include 785 MB of data (compressed) in this repository.

I have the data files in `../data`, this being a reference from the source root. I also created a small dataset to test the code before running the full thing with
```bash
$ head -n 20 IPGOD.IPGOD122B_PAT_ABSTRACTS.csv > pat_abstracts.csv
```

### First task

Reading from a stream.

### Second task

Being able to inspect the stream using something like `take` or `show` with indexing. I assume I would be doing it in `GHCi`.

### Third task

Extracting relevant info from unstructured text, such as addresses. That's a big part of what I do for work, and the main motivation for looking beyond Python. I want to move away from regular expressions and do it fast.

### Fourth task

GROUP BY

### At some point

- Encoding results back into an output file.

Finally, I eagerly welcome help to move this forward. Get in touch!
