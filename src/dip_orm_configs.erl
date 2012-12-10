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
	 get_db_model_config/1,
	 fill_links/1
	]).

-export([
	 find_model/2,
	 get_model_field/2,

	 
	 field/2,
	 model/2,
	 link/2,
	 
	 find_link/2,
	 link_to_join/1
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
	      db_link/0,
	      global_config/0
	     ]).

%% ===================================================================
%%% API
%% ===================================================================

find_model(ModelName,Models) ->
    do([error_m ||
	   ModelName2 <- not_null_binary(ModelName),
	   find_model_(ModelName2,Models)
	      ]).

find_model_(ModelName,Models) ->
    case lists:keyfind(ModelName,#model.name,Models) of
	#model{} = Model ->
	    {ok,Model};
	false ->
	    Reason = dip_utils:template("Unknown model: ~s",[ModelName]), 
	    {error,Reason}
    end.

% find_field(FieldName,#model{fields=Fields} = Model) ->
%     case lists:keyfind(FieldName,#field.name,Fields) of
% 	#field{} = Field ->
% 	    {ok,Field};
% 	false ->
% 	    {error,undefined}
%     end.

find_link(#model{name=LocalModelName,links=Links},
	  #model{name=RemoteModelName}) ->
    FoldFun = fun(#one_to_many{remote_model=R}=Link,_Acc) when R =:= RemoteModelName->
		      {ok,Link};
		 (#many_to_one{remote_model=R}=Link,_Acc) when R =:= RemoteModelName ->
		      {ok,Link};
		 (#many_to_many{remote_model=R}=Link,_Acc) when R =:= RemoteModelName ->
		      {ok,Link};
		 (_,Acc) ->
		      Acc
	      end,
    Reason = dip_utils:template("No model '~s' is linked to '~s'",[RemoteModelName,LocalModelName]),
    lists:foldl(FoldFun,{error,Reason},Links).

link(remote_model,#one_to_many{remote_model=Model}) -> Model;
link(remote_model,#many_to_one{remote_model=Model}) -> Model;
link(remote_model,#many_to_many{remote_model=Model}) -> Model.
    
		      
    
get_model_field(FieldName,ModelConfig) ->
    do([error_m ||
	   FieldName2 <- not_null_binary(FieldName),
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
model(safe_delete,#model{options=#options{deleted_flag_name=Flag}}) -> Flag.
   

link_to_join(#one_to_many{local_table=LocalTable,
			  local_id_alias=LocalIDAlias,
			  remote_table=RemoteTable,
			  remote_id_alias=RemoteIDAlias}) ->
    join_str(LocalTable,LocalIDAlias,RemoteTable,RemoteIDAlias);
link_to_join(#many_to_one{local_table=LocalTable,
			  local_id_alias=LocalIDAlias,
			  remote_table=RemoteTable,
			  remote_id_alias=RemoteIDAlias}) ->
    join_str(LocalTable,LocalIDAlias,RemoteTable,RemoteIDAlias);
link_to_join(#many_to_many{local_table=LocalTable,
			   local_id_alias=LocalIDAlias,
			   link_local_id_alias=LinkLocalIDAlias,
			   link_table=LinkTable,
			   link_remote_id_alias=LinkRemoteIDAlias,
			   remote_table=RemoteTable,
			   remote_id_alias=RemoteIDAlias}) ->
    [
     join_str(LocalTable,LocalIDAlias,LinkTable,LinkLocalIDAlias),
     join_str(LinkTable,LinkRemoteIDAlias,RemoteTable,RemoteIDAlias)
    ].

join_str(LocalTable,LocalID,RemoteTable,RemoteID) ->
    [" JOIN \"",RemoteTable,"\" ON \"",RemoteTable,"\".\"",RemoteID,"\" = \"",LocalTable,"\".\"",LocalID,"\""].

			  

%% ===================================================================
%%% API
%% ===================================================================

get_global_config(Config) ->
    do([error_m ||
	   Opts <- default(option,dip_orm_options,Config,[]),
	   validators(dip_orm_options,Opts,[fun is_list/1]),
	   ConfigsFolder_ <- default(option,configs_folder,Opts,<<"priv/models">>),
	   ConfigsFolder <- not_null_binary(ConfigsFolder_),

	   OutputSrcFolder_ <- default(option,output_src_folder,Opts,<<"src/">>),
	   OutputSrcFolder <- not_null_binary(OutputSrcFolder_),

	   Suffix_ <- default(option,config_suffix,Opts,<<".cfg">>),
	   Suffix <- not_null_binary(Suffix_),
	   return(
	     #global_config{configs_folder=ConfigsFolder,
			    output_src_folder=OutputSrcFolder,
			    config_suffix=Suffix
			   })
	      ]).

get_db_model_config(Config) ->
    do([error_m ||
	   Opts <- default(option,db_models,Config,[]),
	   validators(db_models,Opts,[fun is_list/1]),
	   return(Opts)]).
    

get_config({Name,Options},
	   #global_config{
	     configs_folder=ConfigsFolder,
	     config_suffix=Suffix
	    }) ->
    do([error_m ||
	   validators(options,Options,[fun is_list/1]),
	   ModelName <- not_null_atom(Name),
	   ConfigName <- default(option,config,Options,atom_to_binary_with_suffix(ModelName,Suffix)),
	   ConfigName2 <- not_null_binary(ConfigName),
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

fill_links(Models) ->
    Dict = models_to_dict(Models),
    do([error_m ||
	   Dict2 <- fill_links_db_options(Dict),
	   Dict3 <- fill_one_to_many_links(Dict2),
	   Dict4 <- fill_many_to_many_links(Dict3),
	   print_dict_links(Dict4),
	   return(dict_to_models(Dict4))
	      ]).

models_to_dict(Models) ->
    Names = [Name || #model{name=Name} <- Models],
    Proplist = lists:zip(Names,Models),
    dict:from_list(Proplist).

dict_to_models(Dict) ->
    Proplist = dict:to_list(Dict),
    {_,Models} = lists:unzip(Proplist),
    Models.

print_dict_links(Dict) ->
    Models = dict_to_models(Dict),
    [?DBG("~s: ~p",[Name,Links]) || #model{name=Name,links=Links} <- Models],
    ?DBG(" ~n =========================================== ~n ").

fill_links_db_options(ModelsDict) ->
    FoldFun = fun(#model{name=LocalModelName,links=Links} = LocalModel,Dict) ->
		      LocalTable = model(db_table,LocalModel),
		      MapFun = fun(#many_to_one{local_id=LocalID,
						remote_model=RemoteModelName,
						remote_id=RemoteID} = Link) ->
				       do([error_m ||
					      RemoteModel <- get_model_from_dict(RemoteModelName,Dict),
					      LocalField <- find_field(LocalID,LocalModel),
					      RemoteField <- find_field(RemoteID,RemoteModel),
					      return(Link#many_to_one{
						       local_model = LocalModelName,
						       local_table = LocalTable,
						       local_id_alias = field(db_alias,LocalField),
						       remote_table = model(db_table,RemoteModel),
						       remote_id_alias = field(db_alias,RemoteField)
						      })
						 ])
			       end,
		      do([error_m ||
			     NewLinks <- dip_utils:success_map(MapFun,Links),
			     Dict2 <- return(dict:store(LocalModelName,LocalModel#model{links=NewLinks},Dict)),
			     return(Dict2)
				])
	      end,
    Models = dict_to_models(ModelsDict),
    dip_utils:success_fold(FoldFun,ModelsDict,Models).
		     
    
fill_one_to_many_links(ModelsDict) ->
    FoldFun = fun(#model{links=LocalLinks} = LocalModel,ModelsDict1) ->
		      SubFoldFun = fun(#many_to_one{local_model=LocalModelName,
						    local_id=LocalID,
						    remote_model=RemoteModelName,
						    remote_id=RemoteID,

						    local_table=LocalTable,
						    local_id_alias=LocalIDAlias,
						    remote_table=RemoteTable,
						    remote_id_alias=RemoteIDAlias
						   },
				       ModelsDict2) ->
					   do([error_m ||
						  RemoteModel <- get_model_from_dict(RemoteModelName,ModelsDict2),  
						  LocalField <- find_field(LocalID,LocalModel),
						  RemoteField <- find_field(RemoteID,RemoteModel),
						  check_fields(LocalField,RemoteField),
						  Link <- return(#one_to_many{
						    local_model=RemoteModelName,
						    local_id=RemoteID,
						    remote_model=LocalModelName,
						    remote_id=LocalID,

						    local_table=RemoteTable,
						    local_id_alias=RemoteIDAlias,
						    remote_table=LocalTable,
						    remote_id_alias=LocalIDAlias
						   }),
						  return(dict:store(RemoteModelName,append_link(Link,RemoteModel),ModelsDict2))
						     ]);
				      (_,ModelsDict2) ->
					   ModelsDict2
				   end,
		      dip_utils:success_fold(SubFoldFun,ModelsDict1,LocalLinks)
	      end,
    Models = dict_to_models(ModelsDict),
    dip_utils:success_fold(FoldFun,ModelsDict,Models).

append_link(Link,#model{links=Links} = Model) ->
    Model#model{links=[Link|Links]}.

fill_many_to_many_links(ModelsDict) ->
    FoldFun = fun(LinkModel,ModelsDict1) ->
		      ManyToManyLinks = get_many_to_many_links(LinkModel),
		      SubFoldFun = fun(#many_to_many{local_model=RemoteModelName_1,
						     local_id = RemoteID_1,
						     link_local_id = LinkID_1,
						     link_remote_id = LinkID_2,
						     remote_model = RemoteModelName_2,
						     remote_id = RemoteID_2} = Link_1,ModelsDict2) ->
					   do([error_m ||
						  RemoteModel_1 <- get_model_from_dict(RemoteModelName_1,ModelsDict2),
						  RemoteModel_2 <- get_model_from_dict(RemoteModelName_2,ModelsDict2),
						  RemoteField_1 <- find_field(RemoteID_1,RemoteModel_1),
						  LinkField_1 <- find_field(LinkID_1,LinkModel),
						  check_fields(RemoteField_1,LinkField_1),
						  
						  RemoteField_2 <- find_field(RemoteID_2,RemoteModel_2),
						  LinkField_2 <- find_field(LinkID_2,LinkModel),
						  check_fields(RemoteField_2,LinkField_2),
						  Link_2 <- return(inverse_many_to_many(Link_1)),
						  ModelsDict3 <- return(dict:store(RemoteModelName_1,append_link(Link_1,RemoteModel_1),ModelsDict2)),
						  return(dict:store(RemoteModelName_2,append_link(Link_2,RemoteModel_2),ModelsDict3))
						     ]);
				      (_,ModelsDict2) ->
					   ModelsDict2
				   end,
		      dip_utils:success_fold(SubFoldFun,ModelsDict1,ManyToManyLinks)
	      end,
    Models = dict_to_models(ModelsDict),
    dip_utils:success_fold(FoldFun,ModelsDict,Models).

get_many_to_many_links(#model{links=Links}) ->
    get_many_to_many_links_(Links,[]).
get_many_to_many_links_([],Acc) ->
    Acc;
get_many_to_many_links_([#many_to_one{local_model=LinkModelName_1,
				      local_id=LinkID_1,
				      remote_model=RemoteModelName_1,
				      remote_id=RemoteID_1,

				      local_table=LinkTable_1,
				      local_id_alias=LinkIDAlias_1,
				      remote_table=RemoteTable_1,
				      remote_id_alias=RemoteIDAlias_1 }|RestLinks],Acc) ->
    FoldFun = fun(#many_to_one{local_model=_LinkModelName_2,
			       local_id=LinkID_2,
			       remote_model=RemoteModelName_2,
			       remote_id=RemoteID_2,

			       local_table=_LinkTable_2,
			       local_id_alias=LinkIDAlias_2,
			       remote_table=RemoteTable_2,
			       remote_id_alias=RemoteIDAlias_2

			      },FoldAcc) ->
		      [#many_to_many{local_model=RemoteModelName_1,
				     local_id = RemoteID_1,
				     link_model = LinkModelName_1,
				     link_local_id = LinkID_1,
				     link_remote_id = LinkID_2,
				     remote_model = RemoteModelName_2,
				     remote_id = RemoteID_2,

				     local_table=RemoteTable_1,
				     local_id_alias=RemoteIDAlias_1,
				     link_table=LinkTable_1,
				     link_local_id_alias=LinkIDAlias_1,
				     link_remote_id_alias=LinkIDAlias_2,
				     remote_table=RemoteTable_2,
				     remote_id_alias=RemoteIDAlias_2
				    }|FoldAcc];
		 (_,FoldAcc) ->
		      FoldAcc
	      end,
    lists:foldl(FoldFun,Acc,RestLinks);
get_many_to_many_links_([_|Rest],Acc) ->
    get_many_to_many_links_(Rest,Acc).
    
inverse_many_to_many(#many_to_many{local_model = RemoteModelName_1,
				   local_id = RemoteID_1,
				   link_model = LinkModelName,
				   link_local_id = LinkID_1,
				   link_remote_id = LinkID_2,
				   remote_model = RemoteModelName_2,
				   remote_id = RemoteID_2,

				   local_table=RemoteTable_1,
				   local_id_alias=RemoteIDAlias_1,
				   link_table=LinkTable,
				   link_local_id_alias=LinkIDAlias_1,
				   link_remote_id_alias=LinkIDAlias_2,
				   remote_table=RemoteTable_2,
				   remote_id_alias=RemoteIDAlias_2
				  }) ->
    #many_to_many{local_model=RemoteModelName_2,
		  local_id = RemoteID_2,
		  link_model = LinkModelName,
		  link_local_id = LinkID_2,
		  link_remote_id = LinkID_1,
		  remote_model = RemoteModelName_1,
		  remote_id = RemoteID_1,

		  local_table=RemoteTable_2,
		  local_id_alias=RemoteIDAlias_2,
		  link_table=LinkTable,
		  link_local_id_alias=LinkIDAlias_2,
		  link_remote_id_alias=LinkIDAlias_1,
		  remote_table=RemoteTable_1,
		  remote_id_alias=RemoteIDAlias_1
		 }.


get_model_from_dict(Key,Dict) ->
    case dict:is_key(Key,Dict) of
	true ->
	    Val = dict:fetch(Key,Dict),
	    {ok,Val};
	false ->
	    {error,{unknown_model,Key}}
    end.

find_field(FieldName,#model{fields=Fields}) ->
    case lists:keyfind(FieldName,#field.name,Fields) of
	false ->
	    {error,{unknown_field,FieldName}};
	Field ->
	    {ok,Field}
    end.

check_fields(#field{name=Name1,record_options=#record_options{type=Type1}},
	     #field{name=Name2,record_options=#record_options{type=Type2}}) ->
    case Type1 =/= Type2 of
	true ->
	    Reason = dip_utils:template("Fields '~s' and '~s' has different types",[Name1,Name2]),
	    {error,{wrong_types,Reason}};
	false ->
	    ok
    end.

%% ===================================================================
%%% Internal functions
%% ===================================================================

normalize_({Filename,Name,Config}) ->
    Res = do([error_m ||
		 Options <- required(option,options,Config),
		 validators(options,Options,[fun is_list/1, no(fun is_empty/1)]),
		 
		 Fields <- required(option,fields,Config),
		 validators(fields,Fields,[fun is_list/1, no(fun is_empty/1)]),
		 
		 Options2 <- normalize_options(Options),
		 Fields2 <- normalize_fields(Fields),
		 ModuleName <- not_null_binary(Name),
		 
		 return(
		   #model{name=ModuleName,
			  fields=Fields2,
			  options=Options2,
			  filename=Filename,
			  links = find_links(Fields2)
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
	   Init <- default(flag,init,CustomOptions,false),
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
	   Link <- not_required(option,link,FieldOptions),
	   DBOptions2 <- set_link(Link,DBOptions),
	   return(
	     Field#field{
	       db_options=DBOptions2#db_options{
			    type=Type,
			    alias=Alias
			   }})
	      ]).

set_link(undefined,DBOptions) ->
    DBOptions2 = DBOptions#db_options{is_link = false},
    {ok,DBOptions2};
set_link({Module,Field},DBOptions) ->
    do([error_m ||
	   Module2 <- not_null_binary(Module),
	   Field2 <- not_null_binary(Field),
	   return(DBOptions#db_options{
		    is_link = true,
		    link = {Module2,Field2}
		   })
		]); 
set_link(_,_DBOptions) ->
    {error,{wrong_format,"link format must be set as {link,{Model,Field}}"}}.

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
	   ValidFlagName <- not_null_binary(FlagName),
	   return(Options#options{
		    deleted_flag_name = ValidFlagName
		   })]).

find_links(Fields) ->
    AccFun = fun(#field{name=Name,
			is_in_database=true,
			db_options=#db_options{
			  is_link=true,
			  link={Model,Field}
			 }}, Acc) ->
		     Link = #many_to_one{
		       local_id=Name,
		       remote_model=Model,
		       remote_id=Field
		      },
		     [Link|Acc];
		(_,Acc) -> Acc
	     end,
    lists:foldl(AccFun,[],Fields).
    

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

or_validators(Name,Option,Validators) ->
    case or_validators_(Option,Validators) of
	ok -> ok;
	{error,_Reason} ->
	    Reason = dip_utils:template("Option ~p is invalid",[Name]),
	    {error,{invalid,Reason}}
    end.
or_validators_(_Option,[]) -> {error,invalid};
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

atom_to_binary_with_suffix(Atom,Suffix) ->
    Bin = list_to_binary(atom_to_list(Atom)),
    <<Bin/binary,Suffix/binary>>.

'if'(true,Then,_) -> Then;
'if'(false,_,Else) -> Else.
    

%% ===================================================================
%%% Tests
%% ===================================================================

-include("test/dip_orm_configs.test").
