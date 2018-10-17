unit google.cloud.speech.v1.Speech;

interface

uses
  Grijjy.ProtocolBuffers, System.SysUtils,
  Ultraware.Grpc;

const
  C_Gprc_Path = '/google.cloud.speech.v1.Speech/';

type
  (*
  // Provides "hints" to the speech recognizer to favor specific words and phrases
  // in the results.
  message SpeechContext {
    // *Optional* A list of strings containing words and phrases "hints" so that
    // the speech recognition is more likely to recognize them. This can be used
    // to improve the accuracy for specific words and phrases, for example, if
    // specific commands are typically spoken by the user. This can also be used
    // to add additional words to the vocabulary of the recognizer. See
    // [usage limits](https://cloud.google.com/speech/limits#content).
    repeated string phrases = 1;
  }
  *)
  TSpeechContext = record
  public
    [Serialize(1)] phrases: array of string;
  end;

  TAudioEncoding = (ENCODING_UNSPECIFIED, LINEAR16, FLAC, MULAW, AMR, AMR_WB, OGG_OPUS, SPEEX_WITH_HEADER_BYTE);
  (*
  // Provides information to the recognizer that specifies how to process the
  // request.
  message RecognitionConfig {
    // Audio encoding of the data sent in the audio message. All encodings support
    // only 1 channel (mono) audio. Only `FLAC` and `WAV` include a header that
    // describes the bytes of audio that follow the header. The other encodings
    // are raw audio bytes with no header.
    //
    // For best results, the audio source should be captured and transmitted using
    // a lossless encoding (`FLAC` or `LINEAR16`). Recognition accuracy may be
    // reduced if lossy codecs, which include the other codecs listed in
    // this section, are used to capture or transmit the audio, particularly if
    // background noise is present.
    enum AudioEncoding {
      // Not specified. Will return result [google.rpc.Code.INVALID_ARGUMENT][google.rpc.Code.INVALID_ARGUMENT].
      ENCODING_UNSPECIFIED = 0;

      // Uncompressed 16-bit signed little-endian samples (Linear PCM).
      LINEAR16 = 1;

      // [`FLAC`](https://xiph.org/flac/documentation.html) (Free Lossless Audio
      // Codec) is the recommended encoding because it is
      // lossless--therefore recognition is not compromised--and
      // requires only about half the bandwidth of `LINEAR16`. `FLAC` stream
      // encoding supports 16-bit and 24-bit samples, however, not all fields in
      // `STREAMINFO` are supported.
      FLAC = 2;

      // 8-bit samples that compand 14-bit audio samples using G.711 PCMU/mu-law.
      MULAW = 3;

      // Adaptive Multi-Rate Narrowband codec. `sample_rate_hertz` must be 8000.
      AMR = 4;

      // Adaptive Multi-Rate Wideband codec. `sample_rate_hertz` must be 16000.
      AMR_WB = 5;

      // Opus encoded audio frames in Ogg container
      // ([OggOpus](https://wiki.xiph.org/OggOpus)).
      // `sample_rate_hertz` must be 16000.
      OGG_OPUS = 6;

      // Although the use of lossy encodings is not recommended, if a very low
      // bitrate encoding is required, `OGG_OPUS` is highly preferred over
      // Speex encoding. The [Speex](https://speex.org/)  encoding supported by
      // Cloud Speech API has a header byte in each block, as in MIME type
      // `audio/x-speex-with-header-byte`.
      // It is a variant of the RTP Speex encoding defined in
      // [RFC 5574](https://tools.ietf.org/html/rfc5574).
      // The stream is a sequence of blocks, one block per RTP packet. Each block
      // starts with a byte containing the length of the block, in bytes, followed
      // by one or more frames of Speex data, padded to an integral number of
      // bytes (octets) as specified in RFC 5574. In other words, each RTP header
      // is replaced with a single byte containing the block length. Only Speex
      // wideband is supported. `sample_rate_hertz` must be 16000.
      SPEEX_WITH_HEADER_BYTE = 7;
    }

    // *Required* Encoding of audio data sent in all `RecognitionAudio` messages.
    AudioEncoding encoding = 1;

    // *Required* Sample rate in Hertz of the audio data sent in all
    // `RecognitionAudio` messages. Valid values are: 8000-48000.
    // 16000 is optimal. For best results, set the sampling rate of the audio
    // source to 16000 Hz. If that's not possible, use the native sample rate of
    // the audio source (instead of re-sampling).
    int32 sample_rate_hertz = 2;

    // *Required* The language of the supplied audio as a
    // [BCP-47](https://www.rfc-editor.org/rfc/bcp/bcp47.txt) language tag.
    // Example: "en-US".
    // See [Language Support](https://cloud.google.com/speech/docs/languages)
    // for a list of the currently supported language codes.
    string language_code = 3;

    // *Optional* Maximum number of recognition hypotheses to be returned.
    // Specifically, the maximum number of `SpeechRecognitionAlternative` messages
    // within each `SpeechRecognitionResult`.
    // The server may return fewer than `max_alternatives`.
    // Valid values are `0`-`30`. A value of `0` or `1` will return a maximum of
    // one. If omitted, will return a maximum of one.
    int32 max_alternatives = 4;

    // *Optional* If set to `true`, the server will attempt to filter out
    // profanities, replacing all but the initial character in each filtered word
    // with asterisks, e.g. "f***". If set to `false` or omitted, profanities
    // won't be filtered out.
    bool profanity_filter = 5;

    // *Optional* A means to provide context to assist the speech recognition.
    repeated SpeechContext speech_contexts = 6;

    // *Optional* If `true`, the top result includes a list of words and
    // the start and end time offsets (timestamps) for those words. If
    // `false`, no word-level time offset information is returned. The default is
    // `false`.
    bool enable_word_time_offsets = 8;
  }
  *)
  TRecognitionConfig = record
  public
    [Serialize(1)] encoding: TAudioEncoding;
    [Serialize(2)] sample_rate_hertz: UInt32;
    [Serialize(3)] language_code: string;
    [Serialize(4)] max_alternatives: UInt32;
    [Serialize(5)] profanity_filter: Boolean;
    [Serialize(6)] speech_contexts: array of TSpeechContext;
    [Serialize(8)] enable_word_time_offsets: Boolean;
  end;


  (*
  // Provides information to the recognizer that specifies how to process the
  // request.
  message StreamingRecognitionConfig {
    // *Required* Provides information to the recognizer that specifies how to
    // process the request.
    RecognitionConfig config = 1;

    // *Optional* If `false` or omitted, the recognizer will perform continuous
    // recognition (continuing to wait for and process audio even if the user
    // pauses speaking) until the client closes the input stream (gRPC API) or
    // until the maximum time limit has been reached. May return multiple
    // `StreamingRecognitionResult`s with the `is_final` flag set to `true`.
    //
    // If `true`, the recognizer will detect a single spoken utterance. When it
    // detects that the user has paused or stopped speaking, it will return an
    // `END_OF_SINGLE_UTTERANCE` event and cease recognition. It will return no
    // more than one `StreamingRecognitionResult` with the `is_final` flag set to
    // `true`.
    bool single_utterance = 2;

    // *Optional* If `true`, interim results (tentative hypotheses) may be
    // returned as they become available (these interim results are indicated with
    // the `is_final=false` flag).
    // If `false` or omitted, only `is_final=true` result(s) are returned.
    bool interim_results = 3;
  }
  *)
  PStreamingRecognitionConfig = ^TStreamingRecognitionConfig;
  TStreamingRecognitionConfig = record
  public
    [Serialize(1)] config: TRecognitionConfig;
    [Serialize(2)] single_utterance: Boolean;
    [Serialize(3)] interim_results: Boolean;
  end;

  (*
  // The top-level message sent by the client for the `StreamingRecognize` method.
  // Multiple `StreamingRecognizeRequest` messages are sent. The first message
  // must contain a `streaming_config` message and must not contain `audio` data.
  // All subsequent messages must contain `audio` data and must not contain a
  // `streaming_config` message.
  message StreamingRecognizeRequest {
    // The streaming request, which is either a streaming config or audio content.
    oneof streaming_request {
      // Provides information to the recognizer that specifies how to process the
      // request. The first `StreamingRecognizeRequest` message must contain a
      // `streaming_config`  message.
      StreamingRecognitionConfig streaming_config = 1;

      // The audio data to be recognized. Sequential chunks of audio data are sent
      // in sequential `StreamingRecognizeRequest` messages. The first
      // `StreamingRecognizeRequest` message must not contain `audio_content` data
      // and all subsequent `StreamingRecognizeRequest` messages must contain
      // `audio_content` data. The audio bytes must be encoded as specified in
      // `RecognitionConfig`. Note: as with all bytes fields, protobuffers use a
      // pure binary representation (not base64). See
      // [audio limits](https://cloud.google.com/speech/limits#content).
      bytes audio_content = 2;
    }
  }
  *)
  TStreamingRecognizeRequest = record
  public
    [Serialize(1)] streaming_config: TStreamingRecognitionConfig;
//    [Serialize(2)] audio_content: TBytes;
    function  Serialize: TBytes;
  end;

  TStreamingRecognizeRequest2 = record
  public
//    [Serialize(1)] streaming_config: TStreamingRecognitionConfig;
    [Serialize(2)] audio_content: TBytes;
    function  Serialize: TBytes;
  end;

  TDuration = record
	// Signed seconds of the span of time. Must be from -315,576,000,000
	// to +315,576,000,000 inclusive. Note: these bounds are computed from:
	// 60 sec/min * 60 min/hr * 24 hr/day * 365.25 days/year * 10000 years
	[Serialize(1)] seconds: int64;
	// Signed fractions of a second at nanosecond resolution of the span
	// of time. Durations less than one second are represented with a 0
	// `seconds` field and a positive or negative `nanos` field. For durations
	// of one second or more, a non-zero value for the `nanos` field must be
	// of the same sign as the `seconds` field. Must be from -999,999,999
	// to +999,999,999 inclusive.
	[Serialize(2)] nanos: int32;
  end;

  (*
  // Word-specific information for recognized words. Word information is only
  // included in the response when certain request parameters are set, such
  // as `enable_word_time_offsets`.
  message WordInfo {
    // *Output-only* Time offset relative to the beginning of the audio,
    // and corresponding to the start of the spoken word.
    // This field is only set if `enable_word_time_offsets=true` and only
    // in the top hypothesis.
    // This is an experimental feature and the accuracy of the time offset can
    // vary.
    google.protobuf.Duration start_time = 1;

    // *Output-only* Time offset relative to the beginning of the audio,
    // and corresponding to the end of the spoken word.
    // This field is only set if `enable_word_time_offsets=true` and only
    // in the top hypothesis.
    // This is an experimental feature and the accuracy of the time offset can
    // vary.
    google.protobuf.Duration end_time = 2;

    // *Output-only* The word corresponding to this set of information.
    string word = 3;
  }
  *)
  TWordInfo = record
  public
    [Serialize(1)] start_time: TDuration;
    [Serialize(2)] end_time: TDuration;
    [Serialize(3)] word: string;
  end;

  (*
  // Alternative hypotheses (a.k.a. n-best list).
  message SpeechRecognitionAlternative {
    // *Output-only* Transcript text representing the words that the user spoke.
    string transcript = 1;

    // *Output-only* The confidence estimate between 0.0 and 1.0. A higher number
    // indicates an estimated greater likelihood that the recognized words are
    // correct. This field is typically provided only for the top hypothesis, and
    // only for `is_final=true` results. Clients should not rely on the
    // `confidence` field as it is not guaranteed to be accurate or consistent.
    // The default of 0.0 is a sentinel value indicating `confidence` was not set.
    float confidence = 2;

    // *Output-only* A list of word-specific information for each recognized word.
    repeated WordInfo words = 3;
  }
  *)
  TSpeechRecognitionAlternative = record
  public
    [Serialize(1)] transcript: string;
//    [Serialize(2)] confidence: Double;
    [Serialize(2)] _confidence: FixedInt32;
    [Serialize(3)] words: array of TWordInfo;
    function confidence: Single;
  end;

  (*
  // A streaming speech recognition result corresponding to a portion of the audio
  // that is currently being processed.
  message StreamingRecognitionResult {
    // *Output-only* May contain one or more recognition hypotheses (up to the
    // maximum specified in `max_alternatives`).
    repeated SpeechRecognitionAlternative alternatives = 1;

    // *Output-only* If `false`, this `StreamingRecognitionResult` represents an
    // interim result that may change. If `true`, this is the final time the
    // speech service will return this particular `StreamingRecognitionResult`,
    // the recognizer will not return any further hypotheses for this portion of
    // the transcript and corresponding audio.
    bool is_final = 2;

    // *Output-only* An estimate of the likelihood that the recognizer will not
    // change its guess about this interim result. Values range from 0.0
    // (completely unstable) to 1.0 (completely stable).
    // This field is only provided for interim results (`is_final=false`).
    // The default of 0.0 is a sentinel value indicating `stability` was not set.
    float stability = 3;
  }
  *)
  TStreamingRecognitionResult = record
  public
    [Serialize(1)] alternatives: array of TSpeechRecognitionAlternative;
    [Serialize(2)] is_final: Boolean;
//    [Serialize(3)] stability: Double;
    [Serialize(3)] _stability: FixedInt32;
    function stability: Single;
  end;

  //https://github.com/google/protobuf/blob/master/src/google/protobuf/any.proto
  (*
  message Any {
    // A URL/resource name that uniquely identifies the type of the serialized
    // protocol buffer message. The last segment of the URL's path must represent
    // the fully qualified name of the type (as in
    // `path/google.protobuf.Duration`). The name should be in a canonical form
    // (e.g., leading "." is not accepted).
    //
    // In practice, teams usually precompile into the binary all types that they
    // expect it to use in the context of Any. However, for URLs which use the
    // scheme `http`, `https`, or no scheme, one can optionally set up a type
    // server that maps type URLs to message definitions as follows:
    //
    // * If no scheme is provided, `https` is assumed.
    // * An HTTP GET on the URL must yield a [google.protobuf.Type][]
    //   value in binary format, or produce an error.
    // * Applications are allowed to cache lookup results based on the
    //   URL, or have them precompiled into a binary to avoid any
    //   lookup. Therefore, binary compatibility needs to be preserved
    //   on changes to types. (Use versioned type names to manage
    //   breaking changes.)
    //
    // Note: this functionality is not currently available in the official
    // protobuf release, and it is not used for type URLs beginning with
    // type.googleapis.com.
    //
    // Schemes other than `http`, `https` (or the empty scheme) might be
    // used with implementation specific semantics.
    //
    string type_url = 1;

    // Must be a valid serialized protocol buffer of the above specified type.
    bytes value = 2;
  }
  *)
  TAny = record
  public
    [Serialize(1)] type_url: string;
    [Serialize(2)] value: TBytes;
  end;

  //https://github.com/googleapis/googleapis/blob/master/google/rpc/status.proto
  (*
  message Status {
    // The status code, which should be an enum value of [google.rpc.Code][google.rpc.Code].
    int32 code = 1;

    // A developer-facing error message, which should be in English. Any
    // user-facing error message should be localized and sent in the
    // [google.rpc.Status.details][google.rpc.Status.details] field, or localized by the client.
    string message = 2;

    // A list of messages that carry the error details.  There is a common set of
    // message types for APIs to use.
    repeated google.protobuf.Any details = 3;
  }
  *)
  TStatus = record
  public
    [Serialize(1)] code: Int32;
    [Serialize(2)] message: string;
    [Serialize(3)] details: array of TAny;
  end;

  (*
  // `StreamingRecognizeResponse` is the only message returned to the client by
  // `StreamingRecognize`. A series of zero or more `StreamingRecognizeResponse`
  // messages are streamed back to the client. If there is no recognizable
  // audio, and `single_utterance` is set to false, then no messages are streamed
  // back to the client.
  //
  // Here's an example of a series of ten `StreamingRecognizeResponse`s that might
  // be returned while processing audio:
  //
  // 1. results { alternatives { transcript: "tube" } stability: 0.01 }
  //
  // 2. results { alternatives { transcript: "to be a" } stability: 0.01 }
  //
  // 3. results { alternatives { transcript: "to be" } stability: 0.9 }
  //    results { alternatives { transcript: " or not to be" } stability: 0.01 }
  //
  // 4. results { alternatives { transcript: "to be or not to be"
  //                             confidence: 0.92 }
  //              alternatives { transcript: "to bee or not to bee" }
  //              is_final: true }
  //
  // 5. results { alternatives { transcript: " that's" } stability: 0.01 }
  //
  // 6. results { alternatives { transcript: " that is" } stability: 0.9 }
  //    results { alternatives { transcript: " the question" } stability: 0.01 }
  //
  // 7. results { alternatives { transcript: " that is the question"
  //                             confidence: 0.98 }
  //              alternatives { transcript: " that was the question" }
  //              is_final: true }
  //
  // Notes:
  //
  // - Only two of the above responses #4 and #7 contain final results; they are
  //   indicated by `is_final: true`. Concatenating these together generates the
  //   full transcript: "to be or not to be that is the question".
  //
  // - The others contain interim `results`. #3 and #6 contain two interim
  //   `results`: the first portion has a high stability and is less likely to
  //   change; the second portion has a low stability and is very likely to
  //   change. A UI designer might choose to show only high stability `results`.
  //
  // - The specific `stability` and `confidence` values shown above are only for
  //   illustrative purposes. Actual values may vary.
  //
  // - In each response, only one of these fields will be set:
  //     `error`,
  //     `speech_event_type`, or
  //     one or more (repeated) `results`.
  message StreamingRecognizeResponse {
    // Indicates the type of speech event.
    enum SpeechEventType {
      // No speech event specified.
      SPEECH_EVENT_UNSPECIFIED = 0;

      // This event indicates that the server has detected the end of the user's
      // speech utterance and expects no additional speech. Therefore, the server
      // will not process additional audio (although it may subsequently return
      // additional results). The client should stop sending additional audio
      // data, half-close the gRPC connection, and wait for any additional results
      // until the server closes the gRPC connection. This event is only sent if
      // `single_utterance` was set to `true`, and is not used otherwise.
      END_OF_SINGLE_UTTERANCE = 1;
    }

    // *Output-only* If set, returns a [google.rpc.Status][google.rpc.Status] message that
    // specifies the error for the operation.
    google.rpc.Status error = 1;

    // *Output-only* This repeated list contains zero or more results that
    // correspond to consecutive portions of the audio currently being processed.
    // It contains zero or more `is_final=false` results followed by zero or one
    // `is_final=true` result (the newly settled portion).
    repeated StreamingRecognitionResult results = 2;

    // *Output-only* Indicates the type of speech event.
    SpeechEventType speech_event_type = 4;
  }
  *)
  TSpeechEventType = (SPEECH_EVENT_UNSPECIFIED, END_OF_SINGLE_UTTERANCE);

  TStreamingRecognizeResponse = record
  public
    [Serialize(1)] error: TStatus;
    [Serialize(2)] results: array of TStreamingRecognitionResult;
    [Serialize(4)] speech_event_type: TSpeechEventType;
    procedure Deserialize(const aData: TBytes);
  end;

  TStreamingRecognizeResponseCallback = reference to procedure(const aStreamingRecognizeResponse: TStreamingRecognizeResponse; aHasData, aClosed: Boolean);

  IStreamingRecognizeRequest_Send = interface(IGrpcStream)
    ['{BB8072A2-3DFE-4415-BBEA-DC530C8C15DB}']
    procedure Send(const aStreamingRecognizeRequest: TStreamingRecognizeRequest);
    procedure CloseSend;
  end;

//  TRecognitionAudio_Content = record
	  //Content []byte `protobuf:"bytes,1,opt,name=content,proto3,oneof"`
//    [Serialize(1)] content: TBytes;
//  end;

  TRecognitionAudio = record
	// The audio source, which is either inline content or a Google Cloud
	// Storage uri.
	//
	// Types that are valid to be assigned to AudioSource:
	//	*RecognitionAudio_Content
	//	*RecognitionAudio_Uri
	//AudioSource isRecognitionAudio_AudioSource `protobuf_oneof:"audio_source"`
  //  [Serialize(1)] audio_source: TRecognitionAudio_Content;    werkt niet met "oneof"?
    [Serialize(1)] content: TBytes;
  end;

  TRecognizeRequest = record
	// *Required* Provides information to the recognizer that specifies how to
	// process the request.
	//Config *RecognitionConfig `protobuf:"bytes,1,opt,name=config" json:"config,omitempty"`
	// *Required* The audio data to be recognized.
	//Audio *RecognitionAudio `protobuf:"bytes,2,opt,name=audio" json:"audio,omitempty"`
    [Serialize(1)] config: TRecognitionConfig;
    [Serialize(2)] audio: TRecognitionAudio;
    function  Serialize: TBytes;
  end;

  TSpeechRecognitionResult = record
	// *Output-only* May contain one or more recognition hypotheses (up to the
	// maximum specified in `max_alternatives`).
	// These alternatives are ordered in terms of accuracy, with the top (first)
	// alternative being the most probable, as ranked by the recognizer.
    [Serialize(1)] alternatives: array of TSpeechRecognitionAlternative;
  end;

  TRecognizeResponse = record
	// *Output-only* Sequential list of transcription results corresponding to
	// sequential portions of audio.
    [Serialize(2)] results: array of TSpeechRecognitionResult;
    procedure Deserialize(const aData: TBytes);
  end;

  ISpeech = interface
    ['{27FB57CB-F004-47E4-A3F1-463672313FCC}']

    function Recognize(const aRequest: TRecognizeRequest): TRecognizeResponse;
     //Recognize(context.Context, *RecognizeRequest) (*RecognizeResponse, error)

    // Performs bidirectional streaming speech recognition: receive results while
    // sending audio. This method is only available via the gRPC API (not REST).
    //rpc StreamingRecognize(stream StreamingRecognizeRequest) returns (stream StreamingRecognizeResponse);
    function  StreamingRecognize(const aStreamingRecognizeRequest: TStreamingRecognizeRequest; const aResponseCallback: TStreamingRecognizeResponseCallback): IStreamingRecognizeRequest_Send;
  end;

  TStreamingRecognizeRequest_Send = class(TGrpcStream, IStreamingRecognizeRequest_Send)
  protected
    {IStreamingRecognizeRequest_Send}
    procedure Send(const aStreamingRecognizeRequest: TStreamingRecognizeRequest);
    procedure CloseSend;
  end;

  TSpeech_Client = class(TGrpcClientHandler, ISpeech)
  protected
    {ISpeech}
    function  Recognize(const aRequest: TRecognizeRequest): TRecognizeResponse;
    function  StreamingRecognize(const aStreamingRecognizeRequest: TStreamingRecognizeRequest; const aResponseCallback: TStreamingRecognizeResponseCallback): IStreamingRecognizeRequest_Send;
  end;

implementation

uses
  Grijjy.Http2;

{ TStreamingRecognizeRequest_Send }

procedure TStreamingRecognizeRequest_Send.CloseSend;
begin
  Stream.DoCloseSend;
end;

procedure TStreamingRecognizeRequest_Send.Send(const aStreamingRecognizeRequest: TStreamingRecognizeRequest);
begin
  Stream.SendData(aStreamingRecognizeRequest.Serialize);
end;

{ TStreamingRecognizeRequest }

function TStreamingRecognizeRequest.Serialize: TBytes;
begin
  Result := TgoProtocolBuffer.Serialize(Self);
end;

{ TSpeech_Client }

function TSpeech_Client.Recognize(const aRequest: TRecognizeRequest): TRecognizeResponse;
var
  recv: TBytes;
begin
  if Client.DoRequest(aRequest.Serialize(), C_Gprc_Path + 'Recognize', recv) then
    Result.Deserialize(recv);
end;

function TSpeech_Client.StreamingRecognize(
  const aStreamingRecognizeRequest: TStreamingRecognizeRequest;
  const aResponseCallback: TStreamingRecognizeResponseCallback): IStreamingRecognizeRequest_Send;
var
  request: IGrpcStream;
  callback: TGrpcCallback;
begin
  if Assigned(aResponseCallback) then
    callback :=
      procedure(const aData: TBytes; aIsStreamClosed: Boolean)
      var
        r: TStreamingRecognizeResponse;
      begin
        if aData <> nil then
          r.Deserialize(aData);
        aResponseCallback(r, aData <> nil, aIsStreamClosed);
      end
  else
    callback := nil;

  request := Client.DoRequest(aStreamingRecognizeRequest.Serialize, C_Gprc_Path + 'StreamingRecognize', callback);
  Result  := TStreamingRecognizeRequest_Send.Create(request);
end;

{ TStreamingRecognizeResponse }

procedure TStreamingRecognizeResponse.Deserialize(const aData: TBytes);
begin
  TgoProtocolBuffer.Deserialize(Self, aData);
end;

{ TRecognizeRequest }

function TRecognizeRequest.Serialize: TBytes;
begin
  Result := TgoProtocolBuffer.Serialize(Self);
end;

{ TRecognizeResponse }

procedure TRecognizeResponse.Deserialize(const aData: TBytes);
begin
  TgoProtocolBuffer.Deserialize(Self, aData);
end;

{ TStreamingRecognizeRequest2 }

function TStreamingRecognizeRequest2.Serialize: TBytes;
begin
  Result := TgoProtocolBuffer.Serialize(Self);
end;

{ TSpeechRecognitionAlternative }

function TSpeechRecognitionAlternative.confidence: Single;
var
  i: FixedInt32;
  s: Single absolute i;
begin
  i := _confidence;
  Result := s;
end;

{ TStreamingRecognitionResult }

function TStreamingRecognitionResult.stability: Single;
var
  i: FixedInt32;
  s: Single absolute i;
begin
  i := _stability;
  Result := s;
end;

end.
