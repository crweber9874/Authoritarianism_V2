# Authoritarianism Book
## Federico, Feldman and Weber


# Intro
This repository holds all the code for the book project. Here is a guide to the directory:

The analysis folder holds everything for the analytic sections of the book, organized by chapter. The folders follow the simple convention in that they generally hold two important files.

**run_models.ipynb* or *run_models.R* executes the models. 

Whereas

**post_process.ipynb** generates the figures and tables reported in the book. There's also a folder called
**clean_data**. It houses the original data files and the various recoded data files used throughout the book.
You would ideally run these first, though all the intermediately produced files are in the repo. Finally, I occasionally drop stan model files into folders, mainly for post-processing. The run time is rather long for some of the more complex models -- nothing crazy, like mainly 30 minutes on an Macbook Pro with M1 Chip -- so I save the output.