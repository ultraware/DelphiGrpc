# SimpleDemo
A simple demo showing 4 different kind of calls:
- sync
- mixed
-- sync request/streaming response
-- streaming request/sync response 
- streaming both ways 

It contains a Windows and Linux server, and a Windows, Linux and Android client (all Delphi XE 10.x, gRPC over Websockets for Android).
It also contains a Golang Client and Server to show it is fully compliant to the official gRPC specs.

# SpeechDemo (Google)
Contains a SpeechServer using the Google Speech API and an Android client which records the audio, streams it to the SpeechServer and receives the streaming result back from Google. Also some work is done to record from a Bluetooth headset and it uses tethering to automatically discover the server in the same network. Native Android API is used to do the low level audio recording and end of sentence detection.

Make sure you have an Google Cloud account and downloaded the private key besides the server executable! See demo readme for details.

First start the server and then start the Android client (make sure both are using the same wifi network). 
Tap on the "start" button and start speaking, it will automatically stop when some silence is detected.

# RealtimeMoveDemo
See "Generator" branch.