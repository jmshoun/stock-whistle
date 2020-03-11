# Load a .WAV File with a single cycle sample of a slide whistle
whistle <- loadSample("~/stock-whistle/data/whistle.wav") %>% 
  sound() %>% 
  apply(2, mean)

# Fit a natural cubic spline to the sample
whistle.spline <- splinefun(x=seq_along(whistle), y=whistle, method="natural")
# Find the exact endpoints of the cycle
low.root <- uniroot(whistle.spline, c(1, 5))$root
high.root <- uniroot(whistle.spline, c(-4, 0) + length(whistle))$root
wavelength <- high.root - low.root
units.per.rad <- wavelength / (2 * pi)

value_to_frequency <- function(value, base.frequency, octave.ratio) {
    frequency.scale <- 1 / (2 * log2(1 + octave.ratio / 2))
    norm.value <- log(value) - median(log(value))
    exp(norm.value * frequency.scale) * base.frequency
}

frequency_to_radians <- function(frequency, time, sample.rate) {
    # Fit a cubic spline to interpolate frequency at each sample point
    log.freq.spline <- splinefun(x=seq_along(frequency)-1, y=log(frequency), method="natural")
    num.samples <- as.integer(time * sample.rate)
    inst.freq <- exp(log.freq.spline((seq_len(num.samples) - 1) / (num.samples - 1)
                                     * (length(frequency) - 1)))
    # Compute first derivative in radians at each sample point
    inst.freq / sample.rate * 2 * pi
}

radians_to_waveform <- function(rads) {
    waveform <- rep(0, length(rads))
    pos <- low.root
    for (i in seq_along(rads)) {
        waveform[i] <- whistle.spline(pos)
        pos <- pos + rads[i] * units.per.rad
        if (pos > high.root) {
            pos <- pos - wavelength
        }
    }
    waveform
}

adsr_envelope <- function(attack, decay, sustain, release, len) {
    envelope <- rep(0, len)
    for (i in seq_along(envelope)) {
        pos <- (i - 1) / (len - 1)
        if (pos < attack) {
            envelope[i] <- pos / attack
        } else if (pos < attack + decay) {
            envelope[i] <- (pos - attack) / (decay - attack) * (1 - sustain) + sustain
        } else if (pos < 1 - release) {
            envelope[i] <- sustain
        } else {
            envelope[i] <- (1 - pos) / release * sustain
        }
    }
    envelope
}

play_value <- function(value, base.frequency, octave.ratio, time, sample.rate) {
    freq <- value_to_frequency(value, base.frequency, octave.ratio)
    rads <- frequency_to_radians(freq, time, sample.rate)
    waveform <- radians_to_waveform(rads) * adsr_envelope(0.03, 0.02, 0.7, 0.03, length(rads))
    as.Sample(0.5 * waveform / max(abs(waveform)))
}
