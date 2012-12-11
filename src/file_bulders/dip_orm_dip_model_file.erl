%%%-------------------------------------------------------------------
%%% @author egobrain <egobrain@linux-ympb>
%%% @copyright (C) 2012, egobrain
%%% @doc
%%%
%%% @end
%%% Created : 28 Nov 2012 by egobrain <egobrain@linux-ympb>
%%%-------------------------------------------------------------------
-module(dip_orm_dip_model_file).

-include("log.hrl").
-include("types.hrl").

-compile({parse_transform,do}).

-export([write/2]).
-define(DBM,dip_orm_model_file).

%% ===================================================================
%%% API
%% ===================================================================

write(#model{name=Name,options=#options{dip=true}} = Model, #global_config{output_dip_src_folder=OutFolder}) ->
    ModuleName = dip_orm_configs:model_to_dip_module(Name),
    IsNew = not dip_orm_file:check_exists(ModuleName,OutFolder),

    RequiredContent = [
		       dip_orm_ast:auto(start),
		       dip_orm_ast:attribute(compile,[{parse_transform,do}]),
		       dip_orm_ast:attribute(compile,[{parse_transform,cut}]),
		       dip_orm_ast:attribute(compile,[{parse_transform,dip_orm}]),
		       dip_orm_ast:attribute(include,["log.hrl"]),

		       dip_orm_ast:spacer("Exports"),
		       dip_orm_ast:export([{handle,3}]),
		       dip_orm_ast:comment("Database interection"),
		       dip_orm_ast:export([
					   {new,0},
					   {get, ?DBM:index_fields_count(Model)+1},
					   {find,2},
					   {save,2},
					   {delete,2}
					  ]),

		       dip_orm_ast:comment("Proplist converters"),
		       dip_orm_ast:export([
					   {from_proplist,1},
					   {from_proplist,2},
					   {from_bin_proplist,1},
					   {from_bin_proplist,2},
					   {to_proplist,1}
					  ]),

		       dip_orm_ast:export([{validator,1},
					   {valid,1}
					  ]),

		       dip_orm_ast:comment("Getters and setters"),
		       ?DBM:export_getters_and_setters(Model),

		       dip_orm_ast:spacer("Getters and setters"),
		       render_getters_and_setters(Model),

		       dip_orm_ast:spacer("CRUD. DB operations"),
		       render_crud(Model),

		       dip_orm_ast:spacer("Validators/Data setters"),
		       render_data_validators(Model),
		       
		       dip_orm_ast:auto(stop)
		      ],
    Content = case IsNew of
		  true ->
		      Header = [dip_orm_ast:module(binary_to_list(ModuleName))],
		      Footer = [
				render_footer(Model)
			       ],
		      Header ++ RequiredContent ++ Footer;
		  false ->
		      RequiredContent
	      end,    
    ResultContent = dip_orm_ast:pretty_print(Content),
    dip_orm_file:write_module(ModuleName,OutFolder,ResultContent,IsNew);
write(_,_) -> ok.


%% ===================================================================
%%% Types
%% ===================================================================


render_crud(#model{name=Name,fields=Fields}) ->
    ModuleName = dip_orm_configs:model_to_module(Name),
    IndexFields = ?DBM:get_index_fields(Fields),
    {ok,Content} =  dip_crud_dtl:render([{model,Name},
					 {model_name,ModuleName},
					 {index_fields,IndexFields}
				    ]),
    dip_orm_ast:raw(Content).

render_getters_and_setters(#model{name=Name,fields=Fields}) ->
    ModuleName = dip_orm_configs:model_to_module(Name),
    RecordFields = ?DBM:get_getters_and_setters_fields(Fields),
    {ok,Content} = dip_getters_and_setters_dtl:render([{model_name,ModuleName},
						       {fields,RecordFields}]),
    dip_orm_ast:raw(Content).


render_data_validators(#model{name=Name}) ->
    ModuleName = dip_orm_configs:model_to_module(Name),
    {ok,Content} = dip_data_validators_dtl:render([{model_name,ModuleName}]),
    dip_orm_ast:raw(Content).

render_footer(#model{name=Name,fields=Fields}) ->
    ModuleName = dip_orm_configs:model_to_module(Name),
    IndexFields = ?DBM:get_index_fields(Fields),
    {ok,Content} = dip_footer_dtl:render([{model_name,ModuleName},
					  {index_fields,IndexFields}
					 ]),
    dip_orm_ast:raw(Content).    
