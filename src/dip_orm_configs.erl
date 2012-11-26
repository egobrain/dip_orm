%%%-------------------------------------------------------------------
%%% @author egobrain <egobrain@linux-ympb>
%%% @copyright (C) 2012, egobrain
%%% @doc
%%%
%%% @end
%%% Created : 22 Nov 2012 by egobrain <egobrain@linux-ympb>
%%%-------------------------------------------------------------------
-module(dip_orm_configs).

-compile({parse_transform,do}).

-export([normalize/1]).
-include("log.hrl").

%% ===================================================================
%%% Types
%% ===================================================================

-record(config,{
	  options :: options(),    % Global options for model
	  fields :: [field()],     % fields with options
	  filename :: field_name(),  % Filename where model stores
	  links :: [db_link()]     % links on other models
	 }).
-record(options,{
	  table :: binary(),        % table_name
	  safe_delete :: boolean(), % Option tells to
	  deleted_flag_name :: binary(), % DB field name where delete flag stores
	  dtw :: boolean()  % Create internal Macroses
	 }).
-record(field,{
	  name :: field_name(), % Field name which will be used to access property

	  is_index :: boolean(),
	  is_required :: boolean(),

	  has_record :: boolean(), % Set to true if field value stores in state record
	  record_options :: record_options(), % Editional record options

	  is_in_database :: boolean(), % Set to true if field value stores in state DB
	  db_options :: db_options() % Editional DB record options
	 }).
-record(db_options,{
	  type :: string | integer | datetime,
	  alias :: binary() % Name of field in DB
	 }).
-record(record_options,{
	  type :: field_type(), % Record type. Usefull for dializer
	  description :: any(), % descriptions which stores near record
	  default_value :: any(), % Default value 
	  mode :: r | w | rw | sr | sw | srsw | rsw | srw, % Access mode
	  getter :: true | false | custom, % create getter or use custom
	  setter :: true | false | custom, % create setter or use custom
	  init :: [field_name()] % Fields which are needed for to init
	 }).	  

-record(one_to_one,{
	  local_id :: field_name(),
	  model :: binary(),
	  remote_id :: field_name()
	 }).

-record(one_to_many,{
	  local_id :: field_name(),
	  model :: model_name(),
	  remote_id   :: field_name()
	 }).

-record(many_to_many,{
	  local_id :: field_name(),
	  link_model :: model_name(),
	  remote_model :: model_name()
	 }).

-type field_type() :: {atom(),atom()} | integer | non_neg_integer | binary.
-type field_name() :: binary().
-type model_name() :: binary().

-type record_options() :: #record_options{}.
-type db_options() :: #db_options{}.
-type options() :: #options{}.
-type config() :: #config{}.
-type field() :: #field{}.
-type db_link() :: #one_to_one{} | #one_to_many{} | #many_to_many{}.

-export_type([
	      config/0,
	      options/0,
	      field/0
	     ]).

%% ===================================================================
%%% API
%% ===================================================================

-spec normalize(FileConfigs) -> {ok,Configs} | {error,Reason} when
    FileConfigs :: {Filename :: binary(), Config :: any()},
    Configs :: [config()],
    Reason :: any().
normalize(Configs) ->
    do([error_m ||
	       dip_utils:success_map(fun normalize_/1,Configs)
	      ]).
    

%% ===================================================================
%%% Internal functions
%% ===================================================================

normalize_({Filename,Config}) ->
    Res = do([error_m ||
		 Options <- required(option,options,Config),
		 validators(options,Options,[fun is_list/1, no(fun is_empty/1)]),

		 Fields <- required(option,fields,Config),
		 validators(fields,Fields,[fun is_list/1, no(fun is_empty/1)]),
		 
		 Options2 <- normalize_options(Options),
		 Fields2 <- normalize_fields(Fields),
		 return(
		   #config{fields=Fields2,
			   options=Options2,
			   filename=Filename,
			   links = []
			  }
		  )
		    ]),
    case Res of
	{error,Reason} ->
	    ?DBG("File: ~s~nError: ~p",[Filename,Reason]);
	_ -> ok
    end,
    Res.

normalize_options(Options) ->
    do([error_m ||
	   Table <- required(option,table,Options),
	   TableName <- not_null_binary(Table),
	   SafeDelete <- default(option_or_flag,safe_delete,Options,false),
	   Dtw <- default(flag,dtw,Options,false),
	   ResOptions <- return(#options{table=TableName,dtw=Dtw}),
	   set_safe_delete(SafeDelete,ResOptions)
	      ]).

normalize_fields(Fields) ->
    dip_utils:success_map(fun normalize_field/1,Fields).

normalize_field({Name,Type,FieldOptions}) ->
    do([error_m ||
	   Name2 <- not_null_binary(Name),
	   Type2 <- valid_record_type(Type),
	   normalize_field_options(Name2,Type2,FieldOptions)
	  ]);
normalize_field(_) ->
    {error,{wrong_format,"Field descrition is invalid"}}.
	   
normalize_field_options(Name,Type,FieldOptions) when is_list(FieldOptions) ->
    Field = #field{
      has_record = true,
      record_options = #record_options{
	getter = true,
	setter = true,
	mode = rw
       },
      is_in_database = true,
      db_options = #db_options{}
     },
    do([error_m ||
	   IsIndex <- default(flag,index,FieldOptions,false),
    	   IsRequired <- default(flag,required,FieldOptions,false),
    	   Descritption <- not_required(option,description,FieldOptions),
	   Default <- not_required(option,default,FieldOptions),
	   Mode <- default(option,mode,FieldOptions,rw),
	   valid_variants(mode,Mode,[r, w, rw, sr, sw, srsw, rsw, srw]),
	   Field2 <- begin
			 RecOpts = Field#field.record_options,
			 RecOpts2 = RecOpts#record_options{
				      type = Type,
				      description = Descritption,
				      default_value = Default,
				      mode = Mode
				     },
			 return(Field#field{
				  name = Name,
				  is_index = IsIndex,
				  is_required = IsRequired,
				  record_options=RecOpts2
			  })
		     end,
	   Field3 <- case default(option_or_flag,custom,FieldOptions,undefined) of
			 {ok,CV} when CV =:= undefined orelse CV =:= false  ->
			     parse_db_options(FieldOptions,Field2#field{
							     has_record = true,
							     is_in_database=true
							    });
			 {ok,true} ->
			     parse_custom_options([],Field2#field{is_in_database=false});
			 {ok,CustomOptions} ->
			     parse_custom_options(CustomOptions,Field2#field{is_in_database=false})
		     end,
	   return(Field3#field{
		    
		   })]);
      
normalize_field_options(_Name,_Type,_FieldOptions) ->
    {error,{wrong_format,"Field options must be valid list"}}.

%% = Field parsing ===================================================

parse_custom_options(CustomOptions,#field{record_options=RecOpts} = Field) ->
    do([error_m ||
	   Getter <- default(option_or_flag,get,CustomOptions,false),
	   valid_variants(get,Getter,[true,false,custom]),
	   Setter <- default(option_or_flag,set,CustomOptions,false),
	   valid_variants(set,Setter,[true,false,custom]),
	   HasRecord <- default(flag,record,CustomOptions,false),
	   Init <- not_required(option,init,CustomOptions),
	   or_validators(init,Init,[undefined,fun is_list/1]),
	   return(
	     Field#field{
	       has_record = HasRecord,
	       record_options = RecOpts#record_options{
				  getter = Getter,
				  setter = Setter,
				  init = Init
				  }
	       })
	  ]).

parse_db_options(FieldOptions,#field{
		   name = Name,
		   record_options=RecOpts,
		   db_options=DBOptions} = Field) ->
    RecType = RecOpts#record_options.type,
    do([error_m ||
	   Type <- default(option,db_type,FieldOptions,default_db_type(RecType)),
	   valid_variants(db_type,Type,[string,integer,datetime]),
	   Alias <- default(option,db_alias,FieldOptions,Name),
	   not_null_binary(Alias),
	   return(
	     Field#field{
	       db_options=DBOptions#db_options{
			    type=Type,
			    alias=Alias
			   }})
	      ]).

set_safe_delete(true,Options) ->
    Options2 = Options#options{
		 safe_delete=true,
		 deleted_flag_name = <<"deleted">>
		},
    {ok,Options2};
set_safe_delete(false,Options) ->
    Options2 = Options#options{
	      safe_delete=false
		},
    {ok,Options2};
set_safe_delete(FlagName,Options) ->
    do([error_m ||
	   ValidFlagName <- not_null_binary(FlagName),
	   return(Options#options{
		    safe_delete=true,
		    deleted_flag_name = ValidFlagName
		   })]).


%% = Getters =========================================================

option(Name,Config) ->
    case lists:keyfind(Name,1,Config) of
	{Name,Value} ->
	    {ok,Value};
	false ->
	    {error,undefined}
    end.

flag(Name,Config) ->
    case proplists:get_value(Name,Config) of
	Val when Val == true orelse Val == false ->
	    {ok,Val};
	undefined ->
	    {error,undefined};
	_ ->
	    Reason = dip_utils:template("~p must be valid flag",[Name]),
	    {error,{wrong_format, Reason}}
    end.

option_or_flag(Name,Config) ->
    case option(Name,Config) of
	{ok,Value} ->
	    {ok,Value};
	{error,undefined} ->
	    flag(Name,Config)
    end.

option_function(option) -> fun option/2;
option_function(flag) -> fun flag/2;
option_function(option_or_flag) -> fun option_or_flag/2.

required(Opt,Name,Config) ->
    OptFun = option_function(Opt),
    case OptFun(Name,Config) of
	{ok,Value} ->
	    {ok,Value};
	{error,undefined} ->
	    {error,{Name,required}}
    end.

not_required(Opt,Name,Config) ->
    default(Opt,Name,Config,undefined).

default(Opt,Name,Config,DefaultValue) ->
    OptFun = option_function(Opt),
    case OptFun(Name,Config) of
	{ok,Value} ->
	    {ok,Value};
	{error,undefined} ->
	    case DefaultValue of
		{ok,DefVal} ->
		    {ok,DefVal};
		{error,Reason} ->
		    {error,Reason};
		_ ->	       
		    {ok,DefaultValue}
	    end;
	{error,Reason} ->
	    {error,Reason}
    end.

%% = Default transformations =========================================

not_null_binary(Escape) when Escape =:= undefined orelse
			     Escape =:= null ->
    {error,{wrong_format,"Can't be null or undefined"}};
not_null_binary(<<"">>) ->
    {error,{wrong_format,"Can't be empty"}};
not_null_binary(Bin) when is_binary(Bin) ->
    {ok,Bin};
not_null_binary(Atom) when is_atom(Atom) ->
    not_null_binary(atom_to_list(Atom));
not_null_binary(List) when is_list(List) ->
    not_null_binary(list_to_binary(List));
not_null_binary(Num) when is_number(Num) ->
    {error,{wrong_format,"Can't be number"}};
not_null_binary(Tuple) when is_tuple(Tuple) ->
    {error,{wrong_format,"Can't be tuple"}}.

not_null_atom(Escape) when Escape =:= undefined orelse
			   Escape =:= null ->
    {error,{wrong_format,"Can't be null or undefined"}};
not_null_atom([]) ->
    {error,{wrong_format,"Can't be empty"}};
not_null_atom(List) when is_list(List) ->
    not_null_atom(list_to_atom(List));
not_null_atom(Bin) when is_binary(Bin) ->
    not_null_atom(binary_to_list(Bin));
not_null_atom(Atom) when is_atom(Atom) ->
    {ok,Atom};
not_null_atom(Num) when is_number(Num) ->
    {error,{wrong_format,"Can't be number"}};
not_null_atom(Tuple) when is_tuple(Tuple) ->
    {error,{wrong_format,"Can't be tuple"}}.

%% ===================================================================
%%% Transformers
%% ===================================================================

default_db_type(integer) -> {ok,integer};
default_db_type(non_neg_integer) -> {ok,integer};
default_db_type(binary) -> {ok,string};
default_db_type(Else) ->
    Reason = dip_utils:template("Unknown default db_type for \"~p\"",[Else]),
    {error,{db_type,Reason}}.

%% ===================================================================
%%% Validators
%% ===================================================================

-spec valid_record_type(Type) -> {ok, FieldType} | {error,Reason} when
    Type :: any(),
    FieldType :: field_type(),
    Reason :: {wrong_format,Descritption :: any()}.
valid_record_type({Module,Type}) ->
    do([error_m ||
	   Module2 <- not_null_atom(Module),
	   Type2 <- not_null_atom(Type),
	   return({Module2,Type2})
		]);
valid_record_type(Type) ->
    case not_null_atom(Type) of
	{ok,Value} ->
	    {ok,Value};
	_ ->
	    ReasonText = dip_utils:template("Wrong record type: ~p",[Type]),
	    {error,{wrong_format,ReasonText}}
    end.


-spec valid_variants(Name,Option,Variants) -> ok | {error,{wrong_format,Reason}} when
   Name :: any(),
   Option :: any(),
   Variants :: [any()],
   Reason :: any().
valid_variants(Name,Value,Variants) ->
    case dip_utils:contains(Value,Variants) of
	exists ->
	    ok;
	not_exists ->
	    Reason = dip_utils:template("Option: ~p mustbe in ~p",[Name,Variants]),
	    {error,{wrong_format,Reason}}
    end.

-spec validators(Name,Option,Funs) -> ok | {error,{wrong_format,Reason}} when
   Name :: any(),
   Option :: any(),
   Funs :: fun((Option) -> boolean()),
   Reason :: any().
validators(_Name,_Option,[]) -> ok;
validators(Name,Option,[F|Funs]) ->
    case F(Option) of
	true ->
	    validators(Name,Option,Funs);
	false ->
	    Reason = dip_utils:template("Option ~p is invalid",[Name]),
	    {error,{invalid,Reason}}
    end.

or_validators(Name,Option,Validators) ->
    case or_validators_(Option,Validators) of
	ok -> ok;
	{error,_Reason} ->
	    Reason = dip_utils:template("Option ~p is invalid",[Name]),
	    {error,{invalid,Reason}}
    end.
or_validators_(Option,[]) -> {error,invalid};
or_validators_(Option,[F|Rest]) when is_function(F) ->
    case F(Option) of
	true -> ok;
	false -> or_validators_(Option,Rest)
    end;
or_validators_(Option,[Option|_Rest]) -> ok;
or_validators_(Option,[_|Rest]) -> or_validators_(Option,Rest).
    

is_empty(<<"">>) -> true;
is_empty("") -> true;
is_empty(_) -> false.
    
no(Fun) when is_function(Fun) ->
    fun(Arg) ->
	    not Fun(Arg)
    end.

%% ===================================================================
%%% Tests
%% ===================================================================

-include("test/dip_orm_configs.test").
