%%%-------------------------------------------------------------------
%%% @copyright (C) 2019, Mathias Green
%%% @doc
%%% Basic statem for testing purposes
%%% @end
%%% Created : 08 Jun 2019 by Mathias Green (flmath)
%%%-------------------------------------------------------------------
-module(tracing_experiments).

-behaviour(gen_statem).

-include_lib("tracing_experiments/include/tracing_experiments.hrl").
-include_lib("public_key/include/public_key.hrl").
%% API
-export([start_link/0, stop/0]).
-export([get_state/0, switch_state/0]).

-export([read_public_binary_key/0,
	 read_public_binary_key/1,
	 generate_ephermal_secret/1,
	 encrypt_line/2,
	 regenerate_key/0]).
	 
%% gen_statem callbacks
-export([init/1, callback_mode/0, light_state/3, encrypted_state/3,
	 terminate/3, code_change/4]).
-define(NAME, ?MODULE).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
start_link() ->
    gen_statem:start_link({global, ?NAME}, ?MODULE, [], []).

switch_state()->
    gen_statem:cast({global, ?NAME}, switch_state).

regenerate_key() -> 
    gen_statem:cast({global, ?NAME}, regenerate_key).


get_state()->
    gen_statem:call({global, ?NAME}, get_value).

stop() ->
    gen_statem:call({global, ?NAME}, stop).

%%%===================================================================
%%% gen_statem callbacks
%%%===================================================================

%%--------------------------------------------------------------------
init([]) ->
    PUK = read_public_binary_key(),
    {GenPubKey, Secret, Salt} = generate_ephermal_secret(PUK),   
    {ok, light_state, #{iterator=>0, publicKey => PUK,
			crypto => {GenPubKey, Secret, Salt}}}.

callback_mode() ->
    state_functions.

%%--------------------------------------------------------------------
light_state(cast, switch_state, #{crypto := Crypto}=State) ->
    #{iterator:=Number}=State,
    traced_function(enter_encrypted_state, Number, Crypto),
    {next_state, encrypted_state, State#{iterator:=Number+1}, ?EncStateWindowLength};
light_state(cast, regenerate_key, #{publicKey := PUK}=State) ->
    {GenPubKey, Secret, Salt} = generate_ephermal_secret(PUK),
    {keep_state, State#{crypto => {GenPubKey, Secret, Salt}}};
light_state(timeout, _Event, State) ->   
    #{iterator:=Number} = State,
    traced_function(ignore_timeout, Number, not_important),
    {keep_state, State#{iterator:=Number+1}};
light_state({call, From}, get_value, State) ->    
    #{iterator:=Number} = State,     
    {next_state, light_state, State, [{reply, From, {ok, light_state, Number}}]}.

encrypted_state(cast, switch_state, #{crypto := Crypto}=State) ->   
    #{iterator:=Number} = State,
    traced_function(enter_light_state, Number, Crypto),
    {next_state, light_state, State#{iterator:=Number+1}};
encrypted_state(cast, regenerate_key,  #{publicKey := PUK} = State) ->
    {GenPubKey, Secret, Salt} = generate_ephermal_secret(PUK),
    {keep_state, State#{crypto => {GenPubKey, Secret, Salt}}, ?EncStateWindowLength};
encrypted_state(timeout, _Event, #{crypto := Crypto} = State) ->   
    #{iterator:=Number} = State,
    traced_function(keep_encrypted_state, Number, Crypto),
    {next_state, encrypted_state, State#{iterator:=Number+1}, ?EncStateWindowLength};
encrypted_state({call, From}, get_value, State) ->  
    #{iterator:=Number} = State,       
    {next_state, encrypted_state, State,
     [{reply, From, {ok, encrypted_state, Number}}, 
      {timeout, ?EncStateWindowLength, back_to_encrypted_state}]}.
%%--------------------------------------------------------------------
terminate(_Reason, _StateName, _State) ->
    ok.

code_change(_OldVsn, StateName, State, _Extra) ->
    {ok, StateName, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
traced_function(EncryptedState, Number,  {GenPubKey, Secret, Salt}) when 
      EncryptedState == keep_encrypted_state; 
      EncryptedState == enter_encrypted_state ->
    Msg = atom_to_list(EncryptedState) ++ "sensitive state " ++ integer_to_list(Number),
    EncryptedMsg = encrypt_line(Msg, {GenPubKey, Secret, Salt}),
    logger:notice("unecrypted "++ Msg),
    logger:notice(EncryptedMsg);

traced_function(StateName, Number, _)->
    logger:notice("io:format called from state ~p number ~p~n", [StateName, Number]).


%%%===================================================================
%%% Encryption functions
%%%===================================================================
read_public_binary_key()->
    RootDir = application:get_env(?MODULE, certs_dir, "certs"),
    Path = RootDir ++ "/public_key.pem",
    read_public_binary_key(Path).

read_public_binary_key(Path)->
    {ok, PemKeyBin} = file:read_file(Path),
    [PubKeyRecord] = public_key:pem_decode(PemKeyBin),
    {ECPointRec, NamedCurve} = public_key:pem_entry_decode(PubKeyRecord),
    {'ECPoint', ECPoint} = ECPointRec,
    {ECPoint, NamedCurve}.

generate_ephermal_secret({PubKeyRec,CN})->
    {GenPubKey, GenPrivateKey} = crypto:generate_key(ecdh, map_curve_name(CN)),
    Secret = crypto:compute_key(ecdh, PubKeyRec, GenPrivateKey, map_curve_name(CN)),
    %% io:format("Shared secret computed: ~p~n", [Secret]),
    Salt = crypto:strong_rand_bytes(8), 
    SecretPbkdf = crypto:pbkdf2_hmac(sha256, Secret, Salt,1,32),
    %% io:format("Shared obkdf secret computed: ~p~n", [SecretPbkdf]),
    %% save as hex remove 0x04 wchich means uncompressed for EC key binary 
    GenPubKeyHex = tl(tl(bin_to_hex(GenPubKey))),
    HexSalt=bin_to_hex(Salt),
    {GenPubKeyHex, SecretPbkdf, HexSalt}.

encrypt_line(Msg, {GenPubKey, Secret, HexSalt}) ->
    IV = crypto:strong_rand_bytes(16),    
    HexIV = bin_to_hex(IV),
    Enc =  base64:encode(crypto:crypto_one_time(aes_256_ctr, Secret, IV, Msg, true)),
    lists:append(["encrypted ",  GenPubKey," ",HexSalt," ",HexIV," ",  binary_to_list(Enc)]).

bin_to_hex(Val) ->
    string:lowercase(
      lists:append(
	[string:right(integer_to_list(X,16),2,$0) || X <- binary_to_list(Val)])).

map_curve_name({namedCurve,{1,3,36,3,3,2,8,1,1,7}}) -> brainpoolP256r1.
    

