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
-export([
	 get_global_config/1,
	 get_config/2,
	 get_db_model_config/1
	]).

-export([
	 get_model_field/2,
	 
	 field/2,
	 model/2
	]).
-include("log.hrl").

%% ===================================================================
%%% Types
%% ===================================================================

-include("types.hrl").

-export_type([
	      model/0,
	      options/0,
	      field/0,
	      record_options/0,
	      db_options/0,
	      global_config/0
	     ]).

%% ===================================================================
%%% API
%% ===================================================================
		      
    
get_model_field(FieldName,ModelConfig) ->
    do([error_m ||
	   FieldName2 <- not_null_binary({"Module fieldname",FieldName},FieldName),
	   get_model_field_(FieldName2,ModelConfig)
	      ]).
get_model_field_(FieldName,#model{fields=Fields}) ->
    case lists:keyfind(FieldName,#field.name,Fields) of
	#field{is_in_database=true} = Field ->
	    {ok,Field};
	#field{is_in_database=false} ->
	    Reason = dip_utils:template("Field '~s' does not stores in database",[FieldName]), 
	    {error,Reason};
	false ->
	    Reason = dip_utils:template("Unknown field: ~s",[FieldName]), 
	    {error,Reason}
    end.

field(name,#field{name=Name}) -> Name;
field(db_type,#field{db_options=#db_options{type=Db_type}}) -> Db_type;
field(db_alias,#field{db_options=#db_options{alias=Db_alias}}) -> Db_alias.

model(name,#model{name=Name}) -> Name;
model(db_fields,#model{fields=Fields}) ->
    [F || F <- Fields,F#field.is_in_database,((F#field.record_options)#record_options.mode)#access_mode.sr];
model(db_table,#model{options=#options{table=Table}}) -> Table;
model(safe_delete,#model{options=#options{deleted_flag_name=Flag}}) -> Flag;
model(dip,#model{options=#options{dip=Dip}}) -> Dip;
model(db_module,#model{name=Name,options=#options{db_module_prefix=Prefix}}) ->
    binary_to_atom(<<Prefix/binary,Name/binary>>);
model(dip_module,#model{name=Name,options=#options{dip_module_prefix=Prefix}}) ->
    binary_to_atom(<<Prefix/binary,Name/binary>>).			  

%% ===================================================================
%%% API
%% ===================================================================

get_global_config(Config) ->
    do([error_m ||
	   Opts <- get_rebar_config(dip_orm_options,Config,[]),
	   validators(dip_orm_options,Opts,[fun is_list/1]),
	   ConfigsFolder_ <- default(option,configs_folder,Opts,<<"priv/models">>),
	   ConfigsFolder <- not_null_binary(configs_folder,ConfigsFolder_),

	   OutputSrcFolder_ <- default(option,output_src_folder,Opts,<<"src/">>),
	   OutputSrcFolder <- not_null_binary(output_src_folder,OutputSrcFolder_),

	   DipSrcFolder_ <- default(option,output_dip_src_folder,Opts,<<"src/">>),
	   DipSrcFolder <- not_null_binary(output_dip_src_folder,DipSrcFolder_),	   

	   Suffix_ <- default(option,config_suffix,Opts,<<".cfg">>),
	   Suffix <- not_null_binary(config_suffix,Suffix_),
	   return(
	     #global_config{configs_folder=ConfigsFolder,
			    output_src_folder=OutputSrcFolder,
			    output_dip_src_folder=DipSrcFolder,
			    config_suffix=Suffix
			   })
	      ]).

get_db_model_config(Config) ->
    do([error_m ||
	   Opts <- get_rebar_config(db_models,Config,[]),
	   validators(db_models,Opts,[fun is_list/1]),
	   return(Opts)]).
    
get_config(Name,GlobalConfig) when is_binary(Name) orelse is_atom(Name)  ->
    Options = [],
    get_config({Name,Options},GlobalConfig);
get_config({Name,Options},
	   #global_config{
	     configs_folder=ConfigsFolder,
	     config_suffix=Suffix
	    }) ->
    do([error_m ||
	   validators(options,Options,[fun is_list/1]),
	   ModelName <- not_null_atom({"Config Name",Name},Name),
	   ConfigName <- default(option,config,Options,atom_to_binary_with_suffix(ModelName,Suffix)),
	   ConfigName2 <- not_null_binary(config,ConfigName),
	   ConfigFile <- return(filename:join(ConfigsFolder,ConfigName2)),
	   ModelConfig <- dip_orm_file:read_config(ConfigFile),
	   normalize_({ConfigFile,ModelName,ModelConfig})
	      ]);
get_config(_,_) ->
    {error,{wrong_format,"Wrong models config description"}}.

-spec normalize(FileConfigs) -> {ok,Configs} | {error,Reason} when
    FileConfigs :: {Filename :: binary(), Config :: any()},
    Configs :: [model()],
    Reason :: any().
normalize(Configs) ->
    do([error_m ||
	       dip_utils:success_map(fun normalize_/1,Configs)
	      ]).

%% ===================================================================
%%% Internal functions
%% ===================================================================

get_rebar_config(Key,RebarConfig,Default) ->
    Value = rebar_config:get(RebarConfig,Key,Default),
    {ok,Value}.

normalize_({Filename,Name,Config}) ->
    Res = do([error_m ||
		 Options <- required(option,options,Config),
		 validators(options,Options,[fun is_list/1, no(fun is_empty/1)]),
		 
		 Fields <- required(option,fields,Config),
		 validators(fields,Fields,[fun is_list/1, no(fun is_empty/1)]),
		 
		 Options2 <- normalize_options(Options),
		 Fields2 <- normalize_fields(Fields),
		 ModuleName <- not_null_binary({"Module name",Name},Name),
		 
		 return(
		   #model{name=ModuleName,
			  fields=Fields2,
			  options=Options2,
			  filename=Filename
			 }
		  )
		    ]),
    case Res of
	{error,Reason} ->
	    ?ERR("File: ~s~nError: ~p",[Filename,Reason]);
	_ -> ok
    end,
    Res.

normalize_options(Options) ->
    do([error_m ||
	   Table <- required(option,table,Options),
	   TableName <- not_null_binary(table,Table),
	   Dip <- default(option_or_flag,dip,Options,false),
	   SafeDelete <- default(option_or_flag,safe_delete,Options,false),
	   Dtw <- default(flag,dtw,Options,false),
	   DbModulePrefix_ <- default(option,db_module_prefix,Options,<<"db_">>),
	   DbModulePrefix <- to_binary(db_module_prefix,DbModulePrefix_),

	   DipModulePrefix_ <- default(option,dip_module_prefix,Options,<<"dip_">>),
	   DipModulePrefix <- to_binary(dip_module_prefix,DipModulePrefix_),

	   ResOptions <- return(#options{table=TableName,
					 dtw=Dtw,
					 dip=Dip,
					 db_module_prefix=DbModulePrefix,
					 dip_module_prefix=DipModulePrefix
					}),
	   set_safe_delete(SafeDelete,ResOptions)
	      ]).

normalize_fields(Fields) ->
    dip_utils:success_map(fun normalize_field/1,Fields).

normalize_field({Name,Type,FieldOptions}) ->
    do([error_m ||
	   Name2 <- not_null_binary({"Field name",Name},Name),
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
	setter = true
       },
      is_in_database = true,
      db_options = #db_options{}
     },
    do([error_m ||
	   IsIndex <- default(flag,index,FieldOptions,false),
    	   IsRequired <- default(flag,required,FieldOptions,false),
    	   Descritption <- not_required(option,description,FieldOptions),
	   Default <- not_required(option,default,FieldOptions),
	   Mode <- default(option,mode,FieldOptions,'if'(IsIndex,rsw,rw)),
	   Validators <- default(option,validators,FieldOptions,[]),
	   Validators2 <- valid_validators(Validators),
	   case IsIndex of
	       true ->
		   valid_variants(mode,Mode,[r, sr, sw, srsw, rsw]);
	       false ->
		   valid_variants(mode,Mode,[r, w, rw, sr, sw, srsw, rsw, srw])
	   end,
	   Field2 <- begin
			 RecOpts = Field#field.record_options,
			 RecOpts2 = RecOpts#record_options{
				      type = Type,
				      description = Descritption,
				      default_value = Default,
				      mode = mode_to_acl(Mode),
				      validators=Validators2
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
			     return(Field2#field{
						has_record = true,
						is_in_database=true,
						from_db=true
					       });
			 {ok,true} ->
			     parse_custom_options([],Field2#field{is_in_database=false});
			 {ok,CustomOptions} ->
			     parse_custom_options(CustomOptions,Field2#field{is_in_database=false})
		     end,
	   Field4 <- case Field3#field.from_db of
			 true ->
			     parse_db_options(FieldOptions,Field3);
			 false ->
			     return(Field3)
		     end,
	   return(Field4)]);
      
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
	   Init <- default(flag,init,CustomOptions,false),
	   DB <- default(flag,db,CustomOptions,false),
	   return(
	     Field#field{
	       from_db = DB,
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
	   valid_variants(db_type,Type,[string,integer,datetime,number,boolean]),
	   Alias <- default(option,db_alias,FieldOptions,Name),
	   Alias2 <- not_null_binary(db_alias,Alias),
	   return(
	     Field#field{
	       db_options=DBOptions#db_options{
			    type=Type,
			    alias=Alias2
			   }})
	      ]).

set_safe_delete(true,Options) ->
    Options2 = Options#options{
		 deleted_flag_name = <<"deleted">>
		},
    {ok,Options2};
set_safe_delete(false,Options) ->
    Options2 = Options#options{
		 deleted_flag_name = undefined
		},
    {ok,Options2};
set_safe_delete(FlagName,Options) ->
    do([error_m ||
	   ValidFlagName <- not_null_binary({"Safe delete flag",FlagName},FlagName),
	   return(Options#options{
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

not_null_binary(Name,Escape) when Escape =:= undefined orelse
			     Escape =:= null ->
    {error,{Name,"Can't be null or undefined"}};
not_null_binary(Name,<<"">>) ->
    {error,{Name,"Can't be empty"}};
not_null_binary(_Name,Bin) when is_binary(Bin) ->
    {ok,Bin};
not_null_binary(Name,Atom) when is_atom(Atom) ->
    not_null_binary(Name,atom_to_list(Atom));
not_null_binary(Name,List) when is_list(List) ->
    not_null_binary(Name,list_to_binary(List));
not_null_binary(_Name,Num) when is_number(Num) ->
    {error,{wrong_format,"Can't be number"}};
not_null_binary(Name,Tuple) when is_tuple(Tuple) ->
    {error,{Name,"Can't be tuple"}}.


to_binary(_Name,Escape) when Escape =:= undefined orelse
		       Escape =:= null ->
    {ok,<<"">>};
to_binary(_Name,Bin) when is_binary(Bin) ->
    {ok,Bin};
to_binary(Name,Atom) when is_atom(Atom) ->
    to_binary(Name,atom_to_list(Atom));
to_binary(Name,List) when is_list(List) ->
    to_binary(Name,list_to_binary(List));
to_binary(Name,Num) when is_number(Num) ->
    {error,{Name,"Can't be number"}};
to_binary(Name,Tuple) when is_tuple(Tuple) ->
    {error,{Name,"Can't be tuple"}}.


not_null_atom(Name,Escape) when Escape =:= undefined orelse
			   Escape =:= null ->
    {error,{Name,"Can't be null or undefined"}};
not_null_atom(Name,[]) ->
    {error,{Name,"Can't be empty"}};
not_null_atom(Name,List) when is_list(List) ->
    not_null_atom(Name,list_to_atom(List));
not_null_atom(Name,Bin) when is_binary(Bin) ->
    not_null_atom(Name,binary_to_list(Bin));
not_null_atom(_Name,Atom) when is_atom(Atom) ->
    {ok,Atom};
not_null_atom(Name,Num) when is_number(Num) ->
    {error,{Name,"Can't be number"}};
not_null_atom(Name,Tuple) when is_tuple(Tuple) ->
    {error,{Name,"Can't be tuple"}}.

%% ===================================================================
%%% Transformers
%% ===================================================================

default_db_type(integer) -> {ok,integer};
default_db_type(non_neg_integer) -> {ok,integer};
default_db_type(float) -> {ok,number};
default_db_type(binary) -> {ok,string};
default_db_type(boolean) -> {ok,boolean};
default_db_type(Else) ->
    Reason = dip_utils:template("Unknown default db_type for \"~p\"",[Else]),
    {error,{db_type,Reason}}.

mode_to_acl(r) -> #access_mode{r=true,sr=true};
mode_to_acl(w) -> #access_mode{w=true,sw=true};
mode_to_acl(rw) -> #access_mode{r=true,sr=true,w=true,sw=true};
mode_to_acl(sr) -> #access_mode{sr=true};
mode_to_acl(sw) -> #access_mode{sw=true};
mode_to_acl(srsw) -> #access_mode{sr=true,sw=true};
mode_to_acl(rsw) -> #access_mode{r=true,sr=true,sw=true};
mode_to_acl(srw) -> #access_mode{sr=true,w=true,sw=true}.

%% ===================================================================
%%% Validators
%% ===================================================================

-spec valid_record_type(Type) -> {ok, FieldType} | {error,Reason} when
    Type :: any(),
    FieldType :: field_type(),
    Reason :: {wrong_format,Descritption :: any()}.
valid_record_type({Module,Type}) ->
    do([error_m ||
	   Module2 <- not_null_atom({"Record type module",Module},Module),
	   Type2 <- not_null_atom({"Record type",Type},Type),
	   return({Module2,Type2})
		]);
valid_record_type(Type) ->
    case not_null_atom({"Record type",Type},Type) of
	{ok,Value} ->
	    {ok,Value};
	_ ->
	    ReasonText = dip_utils:template("Wrong record type: ~p",[Type]),
	    {error,{wrong_format,ReasonText}}
    end.

valid_validators(Validators) when is_list(Validators) ->
    MapFun = fun({M,F}) ->
		      {ok,#validator_function{module=M,function=F,args=[]}};
		 ({M,F,A}) ->
		      {ok,#validator_function{module=M,function=F,args=A}};
		 (Else) ->
		      Reason = dip_utils:template(
				 "validator option must set to {M,F} or {M,F,A} but not to ~s",[Else]
				 ),
		      {error,{wrong_format,Reason}}
	      end,
    dip_utils:success_map(MapFun,Validators),
    {ok,Validators};
valid_validators(_) ->
    Reason = "'validators' option must be valid list",
    {error,{wrong_format,Reason}}.
    

-spec valid_variants(Name,Option,Variants) -> ok | {error,{wrong_format,Reason}} when
   Name :: any(),
   Option :: any(),
   Variants :: [any()],
   Reason :: any().
valid_variants(Name,Value,Variants) ->
    case dip_utils:contains(Value,Variants) of
	true ->
	    ok;
	false ->
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

% or_validators(Name,Option,Validators) ->
%     case or_validators_(Option,Validators) of
% 	ok -> ok;
% 	{error,_Reason} ->
% 	    Reason = dip_utils:template("Option ~p is invalid",[Name]),
% 	    {error,{invalid,Reason}}
%     end.
% or_validators_(_Option,[]) -> {error,invalid};
% or_validators_(Option,[F|Rest]) when is_function(F) ->
%     case F(Option) of
% 	true -> ok;
% 	false -> or_validators_(Option,Rest)
%     end;
% or_validators_(Option,[Option|_Rest]) -> ok;
% or_validators_(Option,[_|Rest]) -> or_validators_(Option,Rest).
    

is_empty(<<"">>) -> true;
is_empty("") -> true;
is_empty(_) -> false.
    
no(Fun) when is_function(Fun) ->
    fun(Arg) ->
	    not Fun(Arg)
    end.

atom_to_binary_with_suffix(Atom,Suffix) ->
    Bin = list_to_binary(atom_to_list(Atom)),
    <<Bin/binary,Suffix/binary>>.

'if'(true,Then,_) -> Then;
'if'(false,_,Else) -> Else.

binary_to_atom(Bin) ->
    list_to_atom(binary_to_list(Bin)).

%% ===================================================================
%%% Tests
%% ===================================================================

-include("test/dip_orm_configs.test").
