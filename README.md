[Dan Garrette]: http://cs.utexas.edu/~dhg
[Jason Baldridge]: http://www.jasonbaldridge.com
[Jason Mielens]: http://jason.mielens.com/


# Low-Resource POS-Tagging: 2014

**Author:** Dan Garrette (dhg@cs.utexas.edu)



This is a rewritten version of the code used in the papers:

> [Learning a Part-of-Speech Tagger from Two Hours of Annotation](http://www.cs.utexas.edu/users/ml/papers/garrette.naacl13.pdf)  
> [Dan Garrette] and [Jason Baldridge]  
> In Proceedings of NAACL 2013  

> [Real-World Semi-Supervised Learning of POS-Taggers for Low-Resource Languages](http://www.cs.utexas.edu/users/ml/papers/garrette.acl13.pdf)  
> [Dan Garrette], [Jason Mielens], and [Jason Baldridge]  
> In Proceedings of ACL 2013  

This archive contains code, written in Scala, for training and tagging using the approach described in the papers.
You do not need to have Scala installed in order to run the software since Scala runs on the Java Virtual Machine (JVM).
Thus, if you have Java installed, you should be able to run the system as described below.

## Setting things up


### Getting the code

Clone the project:

    $ git clone https://github.com/dhgarrette/low-resource-pos-tagging-2014.git
    
    
The rest of these instructions assume starting from the `low-resource-pos-tagging-2014` directory.


### Compile the project

    $ ./compile

NOTE: You will need to be connected to the internet the first time you run this since it will need to download several libraries that are required by the code.


## Running the system

    $ ./run OPTIONS

Training data.  If `rawFile` is given, `toksupFile` or `typesupFile` (or both) must be given.

* `--rawFile`: Location of the unannotated training data.
* `--toksupFile`: Location of the token-supervision training data: annotated sentences.
* `--typesupFile`: Location of the type-supervision training data: annotated words (line breaks don't matter; whitespace is ignored).

Model serialization file.  Required if training data is not given.

* `--modelFile`: Location of the saved model file, either for saving after training, or for retrieving if no training data is given. 

Data to run the tagger on.

* `--inputFile`: Location of an unannotated file to tag.
* `--outputFile`: Output location of the result of tagging the `inputFile`.
* `--evalFile`: Location of an annotated file to evaluate on.

Additional options.

* `--tdCutoff`: Tag dictionary cutoff.  Default: none.
* `--numRawTokens`: Number of raw tokens (complete sentences up to this number of total tokens).  Default: infinite.
* `--labelPropIterations`: Number of iterations for the label propagation procedure. Default `200`
* `--emIterations`: Number of iterations for the HMM EM training procedure. Default `50`
* `--memmIterations`: Number of iterations for the MEMM training procedure. Default `100`
* `--memmCutoff`: Cutoff for number of feature occurrences.  Default `100`

For example:

    $ ./run --rawFile data/raw.txt --toksupFile data/toksup.txt --typesupFile data/typesup.txt --modelFile data/model.ser --memmCutoff 10
    $ ./run --modelFile data/model.ser --inputFile data/input.txt --outputFile data/output.txt
    $ ./run --modelFile data/model.ser --evalFile data/eval.txt


Note: You should set the `JAVA_OPTS` environment variable to increase the available memory:

    export JAVA_OPTS="-Xmx4g"



### Data Format

Unannotated files (`rawFile`, `inputFile`) should be whitespace-separated tokens, one sentence per line:

    the man chases a cat .
    the dog chases a man .

Annotated files (`toksupFile`, `typesupFile`, `evalFile`) should be whitespace-separated tokens, one sentence per line, where each token is `word|tag`:

    the|D man|N sees|V the|D dog|N .|.
    the|D dog|N runs|V .|.





## Universal Tagset Mappings for Malagasy and Kinyarwanda

For those interested in using [Universal POS Tags](http://www.petrovi.de/data/lrec.pdf), please use this mapping, created by [Long Duong](https://sites.google.com/site/longduongunimelb/):

#### Kinyarwanda

| No | Kinyarwanda Tag | Universal Tag | Description |
|---|---|-------|-----------------|
| 1 | , | PUNCT | Comma character |
| 2 | . | PUNCT | Dot character |
| 3 | ADJ | ADJ | Adjective |
| 4 | ADV | ADV | Adverb |
| 5 | C | CONJ | Conjunction |
| 6 | CC | CONJ | Conjunction |
| 7 | DT | DET | Determiner |
| 8 | N | NOUN | Noun |
| 9 | PREP | ADP | Preposition |
| 10 | V | VERB | Verb |
| 11 | X | X | Foreign words |

#### Malagasy

| No | Malagasy Tag | Universal Tag | Description |
|---|---|------|-----------------|
| 1 | , | PUNC | Comma character |
| 2 | : | PUNC | Semi column character |
| 3 | . | PUNC | Dot character |
| 4 | ... | PUNC | Ellipsis |
| 5 | " | PUNC | Quotation character |
| 6 | @-@ | PUNC | Dash character |
| 7 | ADJ | ADJ | Adjective |
| 8 | ADV | ADV | Adverb |
| 9 | C | CONJ | Conjunction |
| 10 | CONJ | CONJ | Conjunction |
| 11 | DT | DET | Determiner |
| 12 | FOC | DET | Focus Marker (similar to determiner) |
| 13 | -LRB- | PUNC | Left Round Bracket |
| 14 | N | NOUN | Noun |
| 15 | NEG | ADV | Negation |
| 16 | PCL | PRT | Particle |
| 17 | PN | NOUN | Proper noun |
| 18 | PREP | ADP | Preposition |
| 19 | PRO | PRON | Pronoun |
| 20 | -RRB- | PUNC | Right Round Bracket |
| 21 | T | VERB | Passive verb |
| 22 | V | VERB | Normal Verb |
| 23 | X | X | Foreign root |
