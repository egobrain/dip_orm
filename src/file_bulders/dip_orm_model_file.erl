%%%-------------------------------------------------------------------
%%% @author egobrain <egobrain@linux-ympb>
%%% @copyright (C) 2012, egobrain
%%% @doc
%%%
%%% @end
%%% Created : 28 Nov 2012 by egobrain <egobrain@linux-ympb>
%%%-------------------------------------------------------------------
-module(dip_orm_model_file).

-include("log.hrl").
-include("types.hrl").

-compile({parse_transform,do}).

-export([write/2]).

-export([index_fields_count/1,
	 export_getters_and_setters/1,
	 render_getters_and_setters/1,
	 get_index_fields/1,
	 get_getters_and_setters_fields/1
	]).

-define(PREFIX,"db_").

%% ===================================================================
%%% API
%% ===================================================================

write(Model, #global_config{output_src_folder=OutFolder}) ->
    ModuleName = dip_orm_configs:model(db_module,Model),
    IsNew = not dip_orm_file:check_exists(ModuleName,OutFolder),

    RequiredContent = [
		       dip_orm_ast:auto(start),
		       dip_orm_ast:attribute(compile,[{parse_transform,dip_orm}]),
		       dip_orm_ast:attribute(compile,[{parse_transform,do}]),
		       dip_orm_ast:attribute(compile,[{parse_transform,cut}]),
		       dip_orm_ast:attribute(include,["log.hrl"]),

		       dip_orm_ast:spacer("TYPES"),
		       render_types(Model),

		       render_dtw(Model),

		       dip_orm_ast:spacer("Exports"),
		       dip_orm_ast:comment("Database interection"),
		       dip_orm_ast:export([
					   {new,0},
					   {get, index_fields_count(Model)},
					   {find,1},
					   {save,1},
					   {delete,1}
					  ]),
		       dip_orm_ast:comment("Getters and setters"),
		       export_getters_and_setters(Model),
		       dip_orm_ast:comment("Proplist converters"),
		       dip_orm_ast:export([
					   {from_proplist,1},
					   {from_proplist,2},
					   {from_bin_proplist,1},
					   {from_bin_proplist,2},
					   {to_proplist,1}
					  ]),
		       dip_orm_ast:comment("Internal functions"),
		       dip_orm_ast:export([
					   {'$meta',1},
					   {'$meta',2},
					   {field_constructor,1} % REMOVE THIS !!!
					  ]),
		       dip_orm_ast:export([
					   {valid,1},
					   {validator,1},
					   {constructor,0},
					   {constructor,1}
					  ]),

		       dip_orm_ast:spacer("Getters and setters"),
		       render_getters_and_setters(Model),

		       dip_orm_ast:spacer("CRUD. DB operations"),
		       render_crud(Model),

		       dip_orm_ast:spacer("Validators/Data setters"),
		       render_data_validators(Model),

		       dip_orm_ast:spacer("Internal functions"),
		       render_internal_functions(Model),

		       dip_orm_ast:auto(stop)
		      ],
    Content = case IsNew of
		  true ->
		      [dip_orm_ast:module(ModuleName)|RequiredContent];
		  false ->
		      RequiredContent
	      end,    
    ResultContent = dip_orm_ast:pretty_print(Content),
    dip_orm_file:write_module(ModuleName,OutFolder,ResultContent,IsNew).


%% ===================================================================
%%% Types
%% ===================================================================

render_types(#model{name=Name,fields=Fields} = Model) ->
    Module = dip_orm_configs:model(db_module,Model),
    RecordFields = get_types_fields(Fields),
    RequiredFields = get_required_fields(Fields),
    {ok,Content} = types_db_dtl:render([
					{model,Name},
					{module,Module},
					{fields,RecordFields},
					{required_fields,RequiredFields}
				       ]),
    dip_orm_ast:raw(Content).

get_required_fields(Fields) ->
    FMapFun = fun(#field{name=Name,
			   has_record=true,
			   is_required=true
			  }) ->
		      {ok,[{name,Name}]};
		 (_) -> filtered
	      end,
    dip_utils:map_filter(FMapFun,Fields).

get_types_fields(Fields) ->
    FMapFun = fun(#field{name=Name,
			 has_record=true,
			 record_options=#record_options{
			   default_value=Default,
			   type=Type,
			   mode=#access_mode{
			     sr=Sr
			    }
			  }
			}) ->
		      {ok,[
			   {name,Name},
			   {default,Default},
			   {type,type_to_string(Type)},
			   {system_read,Sr}
			  ]};
		 (_) -> filtered
	      end,
    dip_utils:map_filter(FMapFun,Fields).

%% ===================================================================
%%% Exports
%% ===================================================================

export_getters_and_setters(#model{fields=Fields}) ->
    Exports = get_exports(Fields,[]),
    dip_orm_ast:export(Exports).
get_exports([],Acc) ->
    lists:reverse([{is_new,1}|Acc]);
get_exports([#field{name=Name,
		    record_options=#record_options{
		      getter=Getter,
		      setter=Setter,
		      mode=#access_mode{
			sr=SystemCanRead,
			sw=SystemCanWrite
		       }}}|Rest],
	    Acc) ->
    Acc2 = case {Getter,SystemCanRead} of
	       {false,_} -> Acc;
	       {_,true} -> [{binary_to_atom(Name),1}|Acc];
	       {_,false} -> Acc
	   end,
    Acc3 = case {Setter,SystemCanWrite} of
	       {false,_} -> Acc2;
	       {_,true} -> [{binary_to_atom(<<"set_",Name/binary>>),2}|Acc2];	   
	       {_,false} -> Acc2
	   end,
    get_exports(Rest,Acc3).
    
%% ===================================================================
%%% Getters and setters
%% ===================================================================
    
render_getters_and_setters(#model{name=Name,fields=Fields} = Model) ->
    Module = dip_orm_configs:model(db_module,Model),
    RecordFields = get_getters_and_setters_fields(Fields),
    {ok,Content} = getters_and_setters_db_dtl:render([{model,Name},
						   {module,Module},
						   {fields,RecordFields}]),
    dip_orm_ast:raw(Content).

get_getters_and_setters_fields(Fields) ->
    FMapFun = fun(#field{name=Name,
			 has_record=true,
			 record_options=#record_options{
			   getter=Getter,
			   setter=Setter,
			   type=Type,
			   mode=Mode}}) ->
		      HasGetter = case {Getter,Mode#access_mode.sr} of
				      {true,true} -> true;
				      _ -> false
				  end,
		      HasSetter = case {Setter,Mode#access_mode.sw} of
				      {true,true} -> true;
				      _ -> false
				  end,
		      {ok,[
			   {name,Name},
			   {type,type_to_string(Type)},
			   {has_getter,HasGetter},
			   {has_setter,HasSetter}
			  ]};
		 (_) -> filtered
	      end,
    dip_utils:map_filter(FMapFun,Fields).


%% ===================================================================
%%% CRUD. DB operations
%% ===================================================================

render_crud(#model{name=Name,fields=Fields} = Model) ->
    Module = dip_orm_configs:model(db_module,Model),
    IndexFields = get_index_fields(Fields),
    {ok,Content} =  crud_db_dtl:render([{model,Name},
				     {module,Module},
				     {index_fields,IndexFields}
				    ]),
    dip_orm_ast:raw(Content).
get_index_fields(Fields) ->
    FMapFun = fun(#field{name=Name,
			 has_record=true,
			 is_index=true,
			 is_in_database=true,
			 record_options=#record_options{
			   type=Type
			  }}) ->
		      {ok,[
			   {name,Name},
			   {type,type_to_string(Type)}
			  ]};
		 (_) -> filtered
	      end,
    dip_utils:map_filter(FMapFun,Fields).


index_fields_count(#model{fields=Fields}) ->
    FoldFun = fun(#field{has_record=true,
			 is_index=true,
			 is_in_database=true}, Acc) -> Acc+1;
		 (_,Acc) -> Acc
	      end,
    lists:foldl(FoldFun,0,Fields).

%% ===================================================================
%%% Validators/Data setters
%% ===================================================================

render_data_validators(#model{name=Name,fields=Fields} = Model) ->
    Module = dip_orm_configs:model(db_module,Model),
    ValidatorFields = get_validator_fields(Fields),
    SetterFields = get_setter_fields(Fields),
    FieldsWhichUserCanRead = get_user_can_read_fields(Fields),
    {ok,Content} = data_validators_db_dtl:render([{model,Name},
					       {module,Module},
					       {fields,ValidatorFields},
					       {setter_fields,SetterFields},
					       {user_can_read_fields,FieldsWhichUserCanRead}
					      ]),
    dip_orm_ast:raw(Content).

get_validator_fields(Fields) ->
    FMapFun = fun(#field{name=Name,
			 has_record=true,
			 is_required=IsRequired,
			 record_options=#record_options{
			   validators=Validators,
			   mode=#access_mode{
			     sr=Sr,
			     sw=Sw
			    }
			  }}) ->
		      Validators2 = [validator_to_string(V) || V <- Validators],
		      IsWriteOnly = {Sr,Sw} =:= {false,true},
		      {ok,[
			   {name,Name},
			   {is_required,IsRequired},
			   {validators,Validators2},
			   {is_write_only,IsWriteOnly}
			  ]};
		 (_) -> filtered
	      end,
    dip_utils:map_filter(FMapFun,Fields).

get_setter_fields(Fields) ->
    FMapFun = fun(#field{name=Name,
			 has_record=true,
			 record_options=#record_options{
			   setter=Setter,
			   type=Type,
			   mode=Mode}}) when Setter =/= false ->   
		      {ok,[
			   {name,Name},
			   {type,dip_utils:template("~p",[Type])},
			   {system_can_write,Mode#access_mode.sw},
			   {user_can_write,Mode#access_mode.w}
			  ]};
		 (_) -> filtered
	      end,
    dip_utils:map_filter(FMapFun,Fields).




get_user_can_read_fields(Fields) ->
    FMapFun = fun(#field{name=Name,
			 has_record=true,
			 record_options=#record_options{
			   mode=#access_mode{r=true}
			  }}) ->
		      {ok,[{name,Name}]};
		 (_) -> filtered
	      end,
    dip_utils:map_filter(FMapFun,Fields).

%% ===================================================================
%%% Internal functoins
%% ===================================================================

render_internal_functions(#model{name=Name,fields=Fields} = Model) ->
    Module = dip_orm_configs:model(db_module,Model),
    ExternalFields = get_external_function_fields(Fields),
    InternalFields = get_internal_function_fields(Fields),
    CustomInitFields = get_custom_init_fields(Fields),
    WriteOnlyFields = get_write_only_fields(Fields),
    TableName = dip_orm_configs:model(db_table,Model),
    SafeDelete = dip_orm_configs:model(safe_delete,Model),
    FieldsStr = get_db_str_fields(TableName,Fields),
    DbFields = get_db_read_fields(Fields),
    {ok,Content} = internal_functions_db_dtl:render(
		     [
		      {model,Name},
		      {module,Module},
		      {table_name,TableName},
		      {external_fields,ExternalFields},
		      {fields,InternalFields},
		      {db_fields,DbFields},
		      {custom_init_fields,CustomInitFields},
		      {write_only_fields,WriteOnlyFields},
		      {delete_flag,SafeDelete},
		      {fields_sql,dip_utils:template("~s",[string:join(FieldsStr,",")])}
		     ]),
    dip_orm_ast:raw(Content).

get_write_only_fields(Fields) ->
    FMapFun = fun(#field{name=Name,
			 is_in_database=true,
			 record_options=#record_options{
					   mode=#access_mode{
						   sw=true,
						   sr=false
						  }}}) ->
		      {ok,[{name,Name}]};
		 (_) -> filtered
	      end,			   
    dip_utils:map_filter(FMapFun,Fields).

get_external_function_fields(Fields) ->
    FMapFun = fun(#field{name=Name,
			 from_db = true,
			 db_options=#db_options{
				       type=Db_type
				      }}) ->
		      {ok,[
			   {name,Name},
			   {db_type,Db_type}
			  ]};
		 (_) -> filtered
	      end,
    dip_utils:map_filter(FMapFun,Fields).


get_internal_function_fields(Fields) ->
    FMapFun = fun(#field{name=Name,
			 is_in_database=true,
			 db_options=#db_options{
			   alias = Alias,
			   type=Db_type
			  }}) ->
		      {ok,[
			   {name,Name},
			   {db_alias,Alias},
			   {db_type,Db_type}
			  ]};
		 (_) -> filtered
	      end,
    dip_utils:map_filter(FMapFun,Fields).



get_custom_init_fields(Fields) ->
    FMapFun = fun(#field{name=Name,
			 has_record=true,
			 record_options=#record_options{
			   init=true
			  }}) ->
		      {ok,[{name,Name}]};
		 (_) -> filtered
	      end,
    dip_utils:map_filter(FMapFun,Fields).


%% ===================================================================
%%% Internal helpers
%% ===================================================================

render_dtw(#model{fields=Fields,options=#options{dtw=true}} = Config) ->
    
    Table = dip_orm_configs:model(db_table,Config),
    SafeDelete = dip_orm_configs:model(safe_delete,Config),
    Db_fields = get_db_fields(Fields),
    FieldsStr = get_db_str_fields(Table,Fields),
    SafeDeleteStr = case SafeDelete of
			undefined -> "";
			SafeField ->
			    dip_utils:template("~s",[[esc(Table),".",esc(SafeField)," = FALSE"]])
		    end,
    {ok,Content} =  dtw_db_dtl:render([
				       {table_name,Table},
				       {fields_str,dip_utils:template("~s",[string:join(FieldsStr,",")])},
				       {safe_delete_str,SafeDeleteStr},
				       {fields,Db_fields}
				      ]),
    dip_orm_ast:raw(Content);
render_dtw(#model{options=#options{dtw=false}}) ->
    dip_orm_ast:raw("").

get_db_fields(Fields) ->
    FMapFun = fun(#field{name=Name,
			 is_in_database=true} = Field)  ->
		      Db_alias = dip_orm_configs:field(db_alias,Field),
		      Db_type = dip_orm_configs:field(db_type,Field),
		      {ok,[{name,Name},
			   {db_alias,Db_alias},
			   {db_type,Db_type}]};
		 (_) -> filtered
	      end,
    dip_utils:map_filter(FMapFun,Fields).


get_db_read_fields(Fields) ->
    FMapFun = fun (#field{name=Name,
			  is_in_database=true,
			  record_options=#record_options{
			    mode=#access_mode{
			      sr=true
			     }}} = Field)  ->
		      Db_alias = dip_orm_configs:field(db_alias,Field),
		      Db_type = dip_orm_configs:field(db_type,Field),
		      {ok,[{name,Name},
			   {db_alias,Db_alias},
			   {db_type,Db_type}]};
		  (_) -> filtered
	      end,
    dip_utils:map_filter(FMapFun,Fields).



get_db_str_fields(TableName,Fields) ->
    TableEsc = esc(TableName),
    FMapFun = fun(#field{is_in_database=true,
			 record_options=#record_options{
					   mode=#access_mode{
						   sr=true
						  }}} = Field)  ->
		      Db_alias = dip_orm_configs:field(db_alias,Field),
		      {ok,[TableEsc,".",esc(Db_alias)]};
		 (_) -> filtered
	      end,
    dip_utils:map_filter(FMapFun,Fields).		      
			     

%% ===================================================================
%%% Internal helpers
%% ===================================================================

type_to_string(Type) ->
    list_to_binary(type_to_string_(Type)).
type_to_string_({Module,Type}) ->
      dip_utils:template("~s:~s()",[Module,Type]);
type_to_string_(Type) ->
      dip_utils:template("~s()",[Type]).

validator_to_string(#validator_function{module=M,
					function=F,
					args=A}) ->
    [{module,M},
     {function,F},
     {args,A}].

binary_to_atom(Bin) ->
    list_to_atom(binary_to_list(Bin)).

esc(Str) ->
    ["\\\"",Str,"\\\""].

