(asdf:load-system :tts)
(tts:init)
(tts:speak "hello world! ")
(tts:letter "a")
(tts:queue "this is test 1. ")
(tts:queue 100)
(tts:force)

