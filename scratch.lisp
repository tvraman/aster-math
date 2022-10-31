(read-aloud s)
(afl:tts-init)
(setq sec (nth 4  (children d)))
(read-aloud sec)
(read-aloud d)
(afl:tts-stop)

(afl:tts-shutdown)
(setq afl:*tts-log* t)
