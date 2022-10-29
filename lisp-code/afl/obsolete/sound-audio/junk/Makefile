NOTES_OBJ = playnotes.o notes.o
CFLAGS = -g
notes: $(NOTES_OBJ)
	cc   $(CFLAGS) $(NOTES_OBJ) -o notes
play-sound-file.o: play-sound-file.c
	cc -c $(CFLAGS) play-sound-file.c -laudio
test-play: test-play-sound-file.c
	cc $(CFLAGS) test-play-sound-file.c -laudio -o test-play
