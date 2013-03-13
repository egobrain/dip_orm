
-record(model,{
	  name :: model_name(),    % Global model name
	  options :: options(),    % Global options for model
	  fields :: [field()],     % fields with options
	  filename :: field_name()  % Filename where model stores
	 }).
-record(options,{
	  table :: table_name(),   % table_name
	  % safe_delete :: boolean(), % Option tells to
	  deleted_flag_name :: binary(), % DB field name where delete flag stores
	  dtw :: boolean(), % Create internal Macroses
	  dip :: boolean(),
	  db_module_prefix :: binary(),
	  dip_module_prefix :: binary()
	 }).
-record(field,{
	  name :: field_name(), % Field name which will be used to access property

	  is_index :: boolean(),
	  is_required :: boolean(),

	  has_record :: boolean(), % Set to true if field value stores in state record
	  record_options :: record_options(), % Editional record options

	  is_in_database :: boolean(), % Set to true if field value stores in state DB
	  from_db :: boolean(), % field can be stored in db
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
	  mode :: access_mode(), % From 'r | w | rw | sr | sw | srsw | rsw | srw'
	  getter :: true | false | custom, % create getter or use custom
	  setter :: true | false | custom, % create setter or use custom
	  init :: true | false, % Fields which are needed for to init
	  validators :: [validator_function()]
	 }).	  

-record(validator_function,{
	  module :: model_name(),
	  function :: function_name(),
	  args :: [any()]
	 }).

-record(global_config,{
	  configs_folder :: binary(),
	  output_src_folder :: binary(),
	  output_dip_src_folder :: binary(),
	  config_suffix :: binary()
	 }).

% r | w | rw | sr | sw | srsw | rsw | srw, % Access mode
-record(access_mode,{
	  r = false :: boolean(),
	  sr = false :: boolean(),
	  w = false :: boolean(),
	  sw = false :: boolean()
	  }).

-type field_type() :: {atom(),atom()} | integer | non_neg_integer | binary.
-type field_name() :: binary().
-type model_name() :: binary().
-type function_name() :: binary().
-type table_name() :: binary().

-type record_options() :: #record_options{}.
-type db_options() :: #db_options{}.
-type options() :: #options{}.
-type model() :: #model{}.
-type field() :: #field{}.
-type access_mode() :: #access_mode{}.
-type validator_function() :: #validator_function{}.

-type global_config() :: #global_config{}.
