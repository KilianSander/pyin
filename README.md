# pYIN R package

*pyin* is an R package which wraps the [pYIN](https://code.soundsoftware.ac.uk/projects/pyin) algorithm (Mauch & Dixon, 2014) for fundamental frequency estimation via [Sonic Annotator](https://vamp-plugins.org/sonic-annotator/) (Cannam et al., 2010) for use in the R environment.

## Installation

```r
install.packages("devtools")
devtools::install_github("sebsilas/pyin")
```

## Usage

```r 
library(pyin)

# First test using our test function:

test <- test_pyin()

# If the pyin setup was successful on your system, 'test' should contain a 10x5 data with the transcribed note events of a demo audio file we have distributed with the package.

# Try your own audio file:

my_audio_transcription <- pyin('/my/file/path/audio.wav')

# If you install the itembankr package, you can compute extra melodic features on this:

devtools::install_github('sebsilas/itembankr')

library(dplyr)
library(itembankr)


my_audio_transcription %>% produce_extra_melodic_features()

# where my_audio_transcription contains a pYIN result.

```

## Notes

See https://vamp-plugins.org/sonic-annotator/ for information about allowed file types etc.

It is possible to also supply transform files, as described there, via the R package version of pYIN (see the `transform_file` argument).

### Compatability

This R package currently supports Windows, Linux, and Mac 64-bit.
If you require support for other systems, please get in touch.

## References

Cannam, C., Sandler, M., Jewell, M. O., Rhodes, C., & d’Inverno, M. (2010). Linked data and you: Bringing music research software into the Semantic Web. *Journal of New Music Research*, *39*(4), 313–325. <https://doi.org/10.1080/09298215.2010.522715>

Mauch, M., & Dixon, S. (2014). PYIN: A fundamental frequency estimator using probabilistic threshold distributions. In *Proceedings of the 2014 IEEE International Conference on Acoustics, Speech and Signal Processing (ICASSP)* (pp. 659–663). IEEE. <https://doi.org/10.1109/ICASSP.2014.6853678>

