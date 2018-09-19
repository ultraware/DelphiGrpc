# SimpleDemo
A simple demo showing 4 different kind of calls:
- sync
- mixed
-- sync request/streaming response
-- streaming request/sync response) 
- streaming both ways 

# SpeechDemo
Contains a SpeechServer using the Google Speech API and an Android client which records the audio, streams it to the SpeechServer and receives the streaming result back from Google. Also some work is done to record from a Bluetooth headset and it uses tethering to automatically discover the server in the same network. Native Android API is used to do the low level audio recording and end of sentence detection. 