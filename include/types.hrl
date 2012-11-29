
-record(config,{
	  name :: model_name(),    % Global model name
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
	  alias :: binary(), % Name of field in DB
	  is_link :: boolean(),
	  link :: {model_name(),field_name()}
	 }).
-record(record_options,{
	  type :: field_type(), % Record type. Usefull for dializer
	  description :: any(), % descriptions which stores near record
	  default_value :: any(), % Default value 
	  mode :: access_mode(), % From 'r | w | rw | sr | sw | srsw | rsw | srw'
	  getter :: true | false | custom, % create getter or use custom
	  setter :: true | false | custom, % create setter or use custom
	  init :: [field_name()], % Fields which are needed for to init
	  validators :: [validator_function()]
	 }).	  

-record(many_to_one,{
	  local_id :: field_name(),
	  model :: model_name(),
	  remote_id   :: field_name()
	 }).

-record(one_to_many,{
	  local_id :: field_name(),
	  model :: model_name(),
	  remote_id   :: field_name()
	 }).

-record(many_to_many,{
	  local_id :: field_name(),
	  link_model :: model_name(),
	  link_local_id :: field_name(),
	  link_remote_id :: field_name(),
	  remote_model :: model_name(),
	  remote_id :: field_name()
	 }).

-record(validator_function,{
	  module :: model_name(),
	  function :: function_name(),
	  args :: [any()]
	 }).

-record(global_config,{
	  configs_folder :: binary(),
	  output_src_folder :: binary(),
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

-type record_options() :: #record_options{}.
-type db_options() :: #db_options{}.
-type options() :: #options{}.
-type config() :: #config{}.
-type field() :: #field{}.
-type db_link() :: #many_to_one{} | #one_to_many{} | #many_to_many{}.
-type access_mode() :: #access_mode{}.
-type validator_function() :: #validator_function{}.

-type global_config() :: #global_config{}.
