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

-export([write/2]).

-define(PREFIX,"db_").

%% ===================================================================
%%% API
%% ===================================================================

write(#config{name=Name} = Config, #global_config{output_src_folder=OutFolder}) ->
    ModuleName = module_atom(Name),
    Content = [
	       dip_orm_ast:module(ModuleName),
	       auto(start),
	       dip_orm_ast:attribute(compile,[{parse_transform,do}]),
	       dip_orm_ast:attribute(compile,[{parse_transform,cut}]),
	       dip_orm_ast:attribute(include,["log.hrl"]),

	       dip_orm_ast:spacer("TYPES"),
	       render_types(Config),
	       
	       dip_orm_ast:spacer("Exports"),
	       dip_orm_ast:comment("Database interection"),
	       dip_orm_ast:export([
				   {new,0},
				   {get, index_fields_count(Config)},
				   {save,1},
				   {delete,1}
				  ]),
	       dip_orm_ast:comment("Getters and setters"),
	       export_getters_and_setters(Config),
	       dip_orm_ast:comment("Proplist converters"),
	       dip_orm_ast:export([
				   {from_proplist,1},
				   {from_proplist,2},
				   {from_bin_proplist,1},
				   {from_bin_proplist,2},
				   {to_proplist,1}
				  ]),
	       dip_orm_ast:export([
				   {valid,1},
				   {validator,1}
				  ]),
	       
	       dip_orm_ast:spacer("Getters and setters"),
	       render_getters_and_setters(Config),

	       dip_orm_ast:spacer("CRUD. DB operations"),
	       render_crud(Config),

	       dip_orm_ast:spacer("Validators/Data setters"),
	       render_data_validators(Config),

	       dip_orm_ast:spacer("Internal functions"),
	       render_internal_functions(Config),
	       
	       auto(stop)
	      ],
    ResultContent = dip_orm_ast:pretty_print(Content),
    dip_orm_file:write_module(ModuleName,OutFolder,ResultContent).


%% ===================================================================
%%% Types
%% ===================================================================

render_types(#config{name=Name,fields=Fields}) ->
    ModuleName = module_atom(Name),
    RecordFields = get_types_fields(Fields),
    {ok,Content} = db_types_dtl:render([{model_name,ModuleName},
					{fields,RecordFields}
				       ]),
    dip_orm_ast:raw(Content).

get_types_fields(Fields) ->
    dip_utils:map_filter(fun get_types_field/1,Fields).
get_types_field(#field{name=Name,
		       has_record=true,
		       record_options=#record_options{
			 default_value=Default,
			 type=Type
			}
		      }) ->
    {ok,[
	 {name,Name},
	 {default,Default},
	 {type,type_to_string(Type)}
	]};
get_types_field(_) -> filtered.

%% ===================================================================
%%% Exports
%% ===================================================================

export_getters_and_setters(#config{fields=Fields}) ->
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
    
render_getters_and_setters(#config{name=Name,fields=Fields}) ->
    ModuleName = module_atom(Name),
    RecordFields = get_getters_and_setters_fields(Fields),
    {ok,Content} = getters_and_setters_dtl:render([{model_name,atom_to_list(ModuleName)},
						   {fields,RecordFields}]),
    dip_orm_ast:raw(Content).

get_getters_and_setters_fields(Fields) ->
    dip_utils:map_filter(fun get_getters_and_setters_field/1,Fields).

get_getters_and_setters_field(#field{name=Name,
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
get_getters_and_setters_field(_) -> filtered.

%% ===================================================================
%%% CRUD. DB operations
%% ===================================================================

render_crud(#config{name=Name,fields=Fields}) ->
    ModuleName = atom_to_list(module_atom(Name)),
    IndexFields = get_index_fields(Fields),
    {ok,Content} =  crud_dtl:render([{model_name,ModuleName},
				     {index_fields,IndexFields}
				    ]),
    dip_orm_ast:raw(Content).
get_index_fields(Fields) ->
    dip_utils:map_filter(fun get_index_field/1,Fields).
get_index_field(#field{name=Name,
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
get_index_field(_) -> filtered.

index_fields_count(#config{fields=Fields}) ->
    FoldFun = fun(#field{has_record=true,
			 is_index=true,
			 is_in_database=true}, Acc) -> Acc+1;
		 (_,Acc) -> Acc
	      end,
    lists:foldl(FoldFun,0,Fields).

%% ===================================================================
%%% Validators/Data setters
%% ===================================================================

render_data_validators(#config{name=Name,fields=Fields}) ->
    ModuleName = atom_to_list(module_atom(Name)),
    ValidatorFields = get_validator_fields(Fields),
    SetterFields = get_setter_fields(Fields),
    FieldsWhichUserCanRead = get_user_can_read_fields(Fields),
    {ok,Content} = data_validators_dtl:render([{model_name,ModuleName},
					       {fields,ValidatorFields},
					       {setter_fields,SetterFields},
					       {user_can_read_fields,FieldsWhichUserCanRead}
					      ]),
    dip_orm_ast:raw(Content).

get_validator_fields(Fields) ->
    dip_utils:map_filter(fun get_validator_field/1,Fields).
get_validator_field(#field{name=Name,
			    has_record=true,
			   is_required=IsRequired,
			    record_options=#record_options{
			      validators=Validators
			     }}) ->
    Validators2 = [validator_to_string(V) || V <- Validators],
    {ok,[
	 {name,Name},
	 {is_required,IsRequired},
	 {validators,Validators2}
	]}.

get_setter_fields(Fields) ->
    dip_utils:map_filter(fun get_setter_field/1,Fields).

get_setter_field(#field{name=Name,
			has_record=true,
			record_options=#record_options{
			  setter=Setter,
			  type=Type,
			  mode=Mode}}) when Setter =/= false ->   
    {ok,[
	 {name,Name},
	 {type,Type},
	 {system_can_write,Mode#access_mode.sw},
	 {user_can_write,Mode#access_mode.w}
	]};
get_setter_field(_) -> filtered.


get_user_can_read_fields(Fields) ->
    dip_utils:map_filter(fun get_user_can_read_field/1,Fields).
get_user_can_read_field(#field{name=Name,
				 has_record=true,
				 record_options=#record_options{
				   mode=#access_mode{r=true}
				  }}) ->
    {ok,[{name,Name}]};
get_user_can_read_field(_) -> filtered.

%% ===================================================================
%%% Internal functoins
%% ===================================================================

render_internal_functions(#config{name=Name,fields=Fields}) ->
    ModuleName = atom_to_list(module_atom(Name)),
    InternalFields = get_internal_function_fields(Fields),
    {ok,Content} = internal_functions_dtl:render([
						  {model_name,ModuleName},
						  {fields,InternalFields}
						 ]),
    dip_orm_ast:raw(Content).
get_internal_function_fields(Fields) ->
    dip_utils:map_filter(fun get_internal_function_field/1,Fields).
get_internal_function_field(#field{name=Name}) ->
    {ok,[{name,Name}]};
get_internal_function_field(_) -> filtered.

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

module_atom(Str) ->
    list_to_atom(
      binary_to_list(
	<<?PREFIX,Str/binary>>)).

auto(start) ->
    Content =
	"%% __autostart__\n"
	"%% >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>\n"
	"%%% Please don't edit this file manualy. This file was autogenerated\n"
	"%%% from configs. Orm setup stores in 'rebar.config'\n"
	"%% >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>\n",
    dip_orm_ast:raw(Content);
auto(stop) ->
    Content =
	"%% <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<\n"
	"%%% End of autogenerated text\n"
	"%% <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<\n"
	"%% __autoend__\n",
    dip_orm_ast:raw(Content).


