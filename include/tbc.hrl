-ifndef(tbc).
-define(tbc,true).

-type answer_callback_query_optionals() :: map().
% -type answer_callback_query_optionals() :: #{
%     <<"text"> => undefined | binary(),
%     <<"show_alert"> => undefined | boolean(),
%     <<"url"> => undefined | binary(),
%     <<"cache_time"> => undefined | integer()
% }.

-type api_response(_Type) :: map().
% -type api_response(Type) :: #{
%     <<"ok">> => boolean(),
%     <<"result">> => undefined | Type,
%     <<"description">> => undefined | binary(),
%     <<"error_code">> => undefined | integer(),
%     <<"parameters">> => undefined | response_parameters()
% }.

-type ban_chat_member_optionals() :: map().
% -type ban_chat_member_optionals() :: #{
%     <<"until_date">> => undefined | integer(),
%     <<"revoke_messages">> => undefined | boolean()
% }.

-type bot_command() :: map().
% -type bot_command() :: #{
%     <<"command">> => binary(),
%     <<"description">> => binary()
% }.

-type bot_command_scope() :: 
    bot_command_scope_default()
    | bot_command_scope_all_private_chats()
    | bot_command_scope_all_group_chats()
    | bot_command_scope_all_chat_administrators()
    | bot_command_scope_chat()
    | bot_command_scope_chat_administrators()
    | bot_command_scope_chat_member().

-type bot_command_scope_default() :: map().
% -type bot_command_scope_default() :: #{
%     <<"type">> => <<"default">>
% }.

-type bot_command_scope_all_private_chats() :: map().
% -type bot_command_scope_all_private_chats() :: #{
%     <<"type">> => <<"all_private_chats">>
% }.

-type bot_command_scope_all_group_chats() :: map().
% -type bot_command_scope_all_group_chats() :: #{
%     <<"type">> => <<"all_group_chats">>
% }.

-type bot_command_scope_all_chat_administrators() :: map().
% -type bot_command_scope_all_chat_administrators() :: #{
%     <<"type">> => <<"all_chat_administrators">>
% }.

-type bot_command_scope_chat() :: map().
% -type bot_command_scope_chat() :: #{
%     <<"type">> => <<"chat">>,
%     <<"chat_id">> => chat_id()
% }.

-type bot_command_scope_chat_administrators() :: map().
% -type bot_command_scope_chat_administrators() :: #{
%     <<"type">> => <<"chat_administrators">>,
%     <<"chat_id">> => chat_id()
% }.

-type bot_command_scope_chat_member() :: map().
% -type bot_command_scope_chat_member() :: #{
%     <<"type">> => <<"chat_member">>,
%     <<"chat_id">> => chat_id(),
%     <<"user_id">> => integer()
% }.

-type callback_game() :: map().
% TODO: "A placeholder, currently holds no information"
% https://core.telegram.org/bots/api#callbackgame
% -type callback_game() :: #{}.

-type callback_query() :: map().
% -type callback_query() :: #{
%     id => binary(),
%     from => user(),
%     message => undefined | message(),
%     inline_message_id => undefined | binary(),
%     chat_instance => undefined | binary(),
%     data => undefined | binary(),
%     game_short_name => undefined | binary()
% }.

-type chat_administrator_rights() :: map().
% -type chat_administrator_rights() :: #{
%     <<"is_anonymous">> => boolean(),
%     <<"can_manage_chat">> => boolean(),
%     <<"can_delete_messages">> => boolean(),
%     <<"can_manage_video_chats">> => boolean(),
%     <<"can_restrict_members">> => boolean(),
%     <<"can_promote_members">> => boolean(),
%     <<"can_change_info">> => boolean(),
%     <<"can_invite_users">> => boolean(),
%     <<"can_post_messages">> => undefined | boolean(),
%     <<"can_edit_messages">> => undefined | boolean(),
%     <<"can_pin_messages">> => undefined | boolean(),
%     <<"can_manage_topics">> => undefined | boolean()
% }.

-type chat() :: map().
% -type chat() :: #{
%     <<"id">> => integer(),
%     <<"type">> => binary(),
%     <<"title">> => undefined | binary(),
%     <<"username">> => undefined | binary(),
%     <<"first_name">> => undefined | binary(),
%     <<"last_name">> => undefined | binary(),
%     <<"is_forum">> => undefined | boolean(),
%     <<"photo">> => undefined | chat_photo(),
%     <<"active_usernames">> => undefined | [binary()],
%     <<"emoji_status_custom_emoji_id">> => undefined | binary(),
%     <<"bio">> => undefined | binary(),
%     <<"has_private_forwards">> => undefined | true,
%     <<"has_restricted_voice_and_video_messages">> => undefined | true,
%     <<"join_to_send_messages">> => undefined | true,
%     <<"join_by_requests">> => undefined | true,
%     <<"description">> => undefined | binary(),
%     <<"invite_link">> => undefined | binary(),
%     <<"pinned_message">> => undefined | message(),
%     <<"permissions">> => undefined | chat_permissions(),
%     <<"slow_mode_delay">> => undefined | integer(),
%     <<"message_auto_delete_time">> => undefined | integer(),
%     <<"has_aggressive_anti_spam_enabled">> => undefined | true,
%     <<"has_hidden_members">> => undefined | true,
%     <<"has_protected_content">> => undefined | true,
%     <<"sticker_set_name">> => undefined | boolean(),
%     <<"can_set_name">> => undefined | binary(),
%     <<"can_set_sticker_set">> => undefined | true,
%     <<"linked_chat_id">> => undefined | integer(),
%     <<"location">> => undefined | chat_location()
% }.

-type chat_id() :: binary() | integer().

-type chat_invite_link() :: map().
% -type chat_invite_link() :: #{
%     <<"invite_link">> => binary(),
%     <<"creator">> => user(),
%     <<"creates_join_request">> => boolean(),
%     <<"is_primary">> => boolean(),
%     <<"is_revoked">> => boolean(),
%     <<"name">> => undefined | binary(),
%     <<"expire_date">> => undefined | integer(),
%     <<"member_limit">> => undefined | integer(),
%     <<"pending_join_request_count">> => undefined | integer()
% }.

-type chat_join_request() :: map().
% -type chat_join_request() :: #{
%     chat => chat(),
%     from => user(),
%     user_chat_id => integer(),
%     date => integer(),
%     bio => undefined | binary(),
%     invite_link => undefined | chat_invite_link()
% }.

-type chat_location() :: map().
% -type chat_location() :: #{
%     <<"location">> => location(),
%     <<"address">> => binary()
% }.

-type chat_member() ::
    chat_member_owner()
    | chat_member_administrator()
    | chat_member_member()
    | chat_member_restricted()
    | chat_member_left()
    | chat_member_banned().

-type chat_member_administrator() :: map().
% -type chat_member_administrator() :: #{
%     <<"status">> => binary(),
%     <<"user">> => user(),
%     <<"can_be_edited">> => boolean(),
%     <<"is_anonymous">> => boolean(),
%     <<"can_manage_chat">> => boolean(),
%     <<"can_delete_messages">> => boolean(),
%     <<"can_manage_video_chats">> => boolean(),
%     <<"can_restrict_members">> => boolean(),
%     <<"can_promote_members">> => boolean(),
%     <<"can_change_info">> => boolean(),
%     <<"can_invite_users">> => boolean(),
%     <<"can_post_messages">> => undefined | boolean(),
%     <<"can_edit_messages">> => undefined | boolean(),
%     <<"can_pin_messages">> => undefined | boolean(),
%     <<"can_manage_topics">> => undefined | boolean(),
%     <<"custom_title">> => undefined | binary()
% }.

-type chat_member_banned() :: map().
% -type chat_member_banned() :: #{
%     <<"status">> => binary(),
%     <<"user">> => user(),
%     <<"until_date">> => integer()
% }.

-type chat_member_left() :: map().
% -type chat_member_left() :: #{
%     <<"status">> => binary(),
%     <<"user">> => user()
% }.

-type chat_member_member() :: map().
% -type chat_member_member() :: #{
%     <<"status">> => binary(),
%     <<"user">> => user()
% }.

-type chat_member_owner() :: map().
% -type chat_member_owner() :: #{
%     <<"status">> => binary(),
%     <<"user">> => user(),
%     <<"is_anonymous">> => boolean(),
%     <<"custom_title">> => undefined | binary()
% }.

-type chat_member_restricted() :: map().
% -type chat_member_restricted() :: #{
%     <<"status">> => binary(),
%     <<"user">> => user(),
%     <<"is_member">> => boolean(),
%     <<"can_send_messages">> => boolean(),
%     <<"can_send_audios">> => boolean(),
%     <<"can_send_documents">> => boolean(),
%     <<"can_send_photos">> => boolean(),
%     <<"can_send_videos">> => boolean(),
%     <<"can_send_video_notes">> => boolean(),
%     <<"can_send_voice_notes">> => boolean(),
%     <<"can_send_polls">> => boolean(),
%     <<"can_send_other_messages">> => boolean(),
%     <<"can_add_web_page_previews">> => boolean(),
%     <<"can_change_info">> => boolean(),
%     <<"can_invite_users">> => boolean(),
%     <<"can_pin_messages">> => boolean(),
%     <<"can_manage_topics">> => boolean(),
%     <<"until_date">> => integer()
% }.

-type chat_member_updated() :: map().
% -type chat_member_updated() :: #{
%     <<"chat">> => chat(),
%     <<"from">> => user(),
%     <<"date">> => integer(),
%     <<"old_chat_member">> => chat_member(),
%     <<"new_chat_member">> => chat_member(),
%     <<"invite_link">> => chat_invite_link()
% }.

-type chat_permissions() :: map().
% -type chat_permissions() :: #{
%     <<"can_send_messages">> => undefined | boolean(),    
%     <<"can_send_audios">> => undefined | boolean(),
%     <<"can_send_documents">> => undefined | boolean(),
%     <<"can_send_photos">> => undefined | boolean(),
%     <<"can_send_videos">> => undefined | boolean(),
%     <<"can_send_video_notes">> => undefined | boolean(),
%     <<"can_send_voice_notes">> => undefined | boolean(),
%     <<"can_send_polls">> => undefined | boolean(),
%     <<"can_send_other_messages">> => undefined | boolean(),
%     <<"can_add_web_page_previews">> => undefined | boolean(),
%     <<"can_change_info">> => undefined | boolean(),
%     <<"can_invite_users">> => undefined | boolean(),
%     <<"can_pin_messages">> => undefined | boolean(),
%     <<"can_manage_topics">> => undefined | boolean()
% }.

-type chosen_inline_result() :: map().
% -type chosen_inline_result() :: #{
%     <<"result_id">> => binary(),
%     <<"from">> => user(),
%     <<"location">> => undefined | location(),
%     <<"inline_message_id">> => undefined | binary(),
%     <<"query">> => undefined | binary()
% }.

-type copy_message_optionals() :: map().
% -type copy_message_optionals() :: #{
%     <<"message_thread_id">> => undefined | integer(),
%     <<"caption">> => undefined | binary(),
%     <<"parse_mode">> => undefined | binary(),
%     <<"caption_entities">> => undefined | [message_entity()],
%     <<"disable_notification">> => undefined | boolean(),
%     <<"protect_content">> => undefined | boolean(),
%     <<"reply_to_message_id">> => undefined | integer(),
%     <<"allow_sending_without_reply">> => undefined | boolean(),
%     <<"reply_markup">> => undefined | keyboard_markup() | force_reply()
% }.

-type create_chat_invite_link_optionals() :: map().
% -type create_chat_invite_link_optionals() :: #{
%     <<"name">> => undefined | binary(),
%     <<"expire_date">> => undefined | integer(),
%     <<"member_limit">> => undefined | integer(),
%     <<"creates_join_request">> => undefined | boolean()
% }.

-type create_forum_topic_optionals() :: map().
% -type create_forum_topic_optionals() :: #{
%     <<"icon_color">> => undefined | integer(),
%     <<"icon_custom_emoji_id">> => undefined | binary()
% }.

-type get_my_commands_optionals() :: map().
% -type get_my_commands_optionals() :: #{
%     <<"scope">> => undefined | bot_command_scope(),
%     <<"language_code">> => undefined | binary()
% }.

-type get_my_default_administrator_rights_optionals() :: map().
% -type get_my_default_administrator_rights_optionals() :: #{
%     <<"for_channels">> => undefined | boolean()
% }.

-type edit_forum_topic_optionals() :: map().
% -type edit_forum_topic_optionals() :: #{
%     <<"name">> => undefined | binary(),
%     <<"icon_custom_emoji_id">> => undefined | binary()
% }.

-type edit_message_live_location_optionals() :: map().
% -type edit_message_live_location_optionals() :: #{
%     <<"chat_id">> => undefined | integer() | binary(),
%     <<"message_id">> => undefined | integer(),
%     <<"inline_message_id">> => undefined | binary(),
%     <<"horizontal_accuracy">> => undefined | float(),
%     <<"heading">> => undefined | integer(),
%     <<"proximity_alert_radius">> => undefined | integer(),
%     <<"reply_markup">> => undefined | inline_keyboard_markup()
% }.

-type file() :: map().
% -type file() :: #{
%     <<"file_id">> => binary(),
%     <<"file_unique_id">> => binary(),
%     <<"file_size">> => undefined | integer(),
%     <<"file_path">> => undefined | binary()
% }.

-type force_reply() :: map().
% -type force_reply() :: #{
%     <<"force_reply">> => boolean(),
%     <<"input_field_placeholder">> => undefined | boolean(),
%     <<"selective">> => undefined | boolean()
% }.

-type forum_topic() :: map().
% -type forum_topic() :: #{
%     <<"message_thread_id">> => integer(),
%     <<"name">> => binary(),
%     <<"icon_color">> => integer(),
%     <<"icon_custom_emoji_id">> => undefined | binary()
% }.

-type forward_message_optionals() :: map().
% -type forward_message_optionals() :: #{
%     <<"message_thread_id">> => undefined | integer(),
%     <<"disable_notification">> => undefined | boolean(),
%     <<"protect_content">> => undefined | boolean()
% }.

-type get_chat_menu_button_optionals() :: map().
% -type get_chat_menu_button_optionals() :: #{
%     <<"chat_id">> => undefined | chat_id()
% }.

-type get_updates_optionals() :: map().
% -type get_updates_optionals() :: #{
%     <<"offset">> => undefined | integer(),
%     <<"limit">> => undefined | integer(),
%     <<"timeout">> => undefined | integer(),
%     <<"allowed_updates">> => undefined | [binary()]
% }.

-type get_user_profile_photos_optionals() :: map().
% -type get_user_profile_photos_optionals() :: #{
%     <<"offset">> => undefined | integer(),
%     <<"limit">> => undefined | integer()
% }.

-type inline_keyboard_button() :: map().
% -type inline_keyboard_button() :: #{
%     <<"text">> => binary(),
%     <<"url">> => undefined | binary(),
%     <<"callback_data">> => undefined | binary(),
%     <<"web_app">> => undefined | web_app_info(),
%     <<"login_url">> => undefined | login_url(),
%     <<"switch_inline_query">> => undefined | binary(),
%     <<"switch_inline_query_current_chat">> => undefined | binary(),
%     <<"callback_game">> => undefined | callback_game(),
%     <<"pay">> => undefined | boolean()
% }.

-type inline_keyboard_markup() :: map().
% -type inline_keyboard_markup() :: #{
%     <<"inline_keyboard">> => [[inline_keyboard_button()]]
% }.

-type inline_query() :: map().
% -type inline_query() :: #{
%     <<"id">> => binary(),
%     <<"from">> => user(),
%     <<"query">> => binary(),
%     <<"offset">> => binary(),
%     <<"chat_type">> => undefined | binary(),
%     <<"location">> => undefined | location()
% }.

-type input_file() :: binary().

-type input_media() :: 
    input_media_audio()
    | input_media_document()
    | input_media_photo()
    | input_media_video().

-type input_media_audio() :: map().
% -type input_media_audio() :: #{
%     <<"type">> => <<"audio">>,
%     <<"media">> => binary(),
%     <<"thumb">> => undefined | binary() | input_file(),
%     <<"caption">> => undefined | binary(),
%     <<"parse_mode">> => undefined | binary(),
%     <<"caption_entities">> => undefined | [message_entity()],
%     <<"duration">> => undefined | integer(),
%     <<"performer">> => undefined | binary(),
%     <<"title">> => undefined | bianry()
% }.

-type input_media_document() :: map().
% -type input_media_document() :: #{
%     <<"type">> => <<"document">>
%     <<"media">> => binary(),
%     <<"thumb">> => undefined | binary() | input_file(),
%     <<"caption">> => undefined | binary(),
%     <<"parse_mode">> => undefined | binary(),
%     <<"caption_entities">> => undefined | [message_entity()],
%     <<"disable_content_type_detection">> => undefined | boolean()
% }.

-type input_media_photo() :: map().
% -type input_media_photo() :: #{
%     <<"type">> => <<"photo">>,
%     <<"media">> => binary(),
%     <<"caption">> => undefined | binary(),
%     <<"parse_mode">> => undefined | binary(),
%     <<"caption_entities">> => undefined | [message_entity()],
%     <<"has_spoiler">> => undefined | boolean()
% }.

-type input_media_video() :: map().
% -type input_media_video() :: #{
%     <<"type">> => <<"video">>,
%     <<"media">> => binary(),
%     <<"thumb">> => undefined | binary() | input_file(),
%     <<"caption">> => undefined | binary(),
%     <<"parse_mode">> => undefined | binary(),
%     <<"caption_entities">> => undefined | [message_entity()],
%     <<"width">> => undefined | integer(),
%     <<"height">> => undefined | integer(),
%     <<"duration">> => undefined | integer(),
%     <<"supports_streaming">> => undefined | boolean(),
%     <<"has_spoiler">> => undefined | boolean()
% }.

-type keyboard_button() :: map().
% -type keyboard_button() :: #{
%     <<"text">> => binary(),
%     <<"request_user">> => undefined | keyboard_button_request_user(),
%     <<"request_chat">> => undefined | keyboard_button_request_chat(),
%     <<"request_contact">> => undefined | boolean(),
%     <<"request_location">> => undefined | boolean(),
%     <<"request_poll">> => undefined | keyboard_button_poll_type(),
%     <<"web_app">> => undefined | web_app_info()
% }.

-type keyboard_button_poll_type() :: map().
% -type keyboard_button_poll_type() :: #{
%     <<"type">> => undefined | binary()
% }.

-type keyboard_button_request_chat() :: map().
% -type keyboard_button_request_chat() :: #{
%     <<"request_id">> => integer(),
%     <<"chat_is_channel">> => boolean(),
%     <<"chat_is_forum">> => undefined | boolean(),
%     <<"chat_has_username">> => undefined | boolean(),
%     <<"chat_is_created">> => undefined | boolean(),
%     <<"user_administrator_rights">> => undefined | chat_administrator_rights(),
%     <<"bot_administrator_rights">> => undefined | chat_administrator_rights(),
%     <<"bot_is_member">> => undefined | boolean()
% }.



-type keyboard_button_request_user() :: map().
% -type keyboard_button_request_user() :: #{
%     <<"request_id">> => integer(),
%     <<"user_is_bot">> => undefined | boolean(),
%     <<"user_is_premium">> => undefined | boolean()
% }.

-type keyboard_markup() :: 
    inline_keyboard_markup()
    | reply_keyboard_markup()
    | reply_keyboard_remove().

-type labeled_price() :: map().
% -type labeled_price() :: #{
%     <<"label">> => binary(),
%     <<"amount">> => integer()
% }.

-type location() :: map().
% -type location() :: #{
%     <<"longitude">> => float(),
%     <<"latitude">> => float(),
%     <<"horizontal_accuracy">> => undefined | float(),
%     <<"live_period">> => undefined | integer(),
%     <<"heading">> => undefined | integer(),
%     <<"proximity_alert_radius">> => undefined | integer()
% }.

-type login_url() :: map().
% -type login_url() :: #{
%     <<"url">> => binary(),
%     <<"forward_text">> => undefined | binary(),
%     <<"bot_username">> => undefined | binary(),
%     <<"request_write_access">> => undefined | boolean()
% }.

-type mask_position() :: map().
% -type mask_position() :: #{
%     <<"point">> => binary(),
%     <<"x_shift">> => float(),
%     <<"y_shift">> => float(),
%     <<"scale">> => float()
% }.

-type menu_button() :: 
    menu_button_commands()
    | menu_button_web_app()
    | menu_button_default().

-type menu_button_commands() :: map().
% -type menu_button_commands :: #{
%     <<"type">> => <<"commands">>
% }.

-type menu_button_default() :: map().
% -type menu_button_default :: #{
%     <<"type">> => <<"default">>
% }.

-type menu_button_web_app() :: map().
% -type menu_button_web_app() :: #{
%     <<"type">> => <<"web_app">>,
%     <<"text">> => binary(),
%     <<"web_app">> => web_app_info()
% }.

-type message() :: map().
% -type message() :: #{
%     <<"chat_id">> => chat_id(),
%     <<"message_thread_id">> => undefined | integer(),
%     <<"text">> => binary(),
%     <<"parse_mode">> => undefined | binary(),
%     <<"entities">> => undefined | [message_entity()],
%     <<"disable_web_page_preview">> => undefined | boolean(),
%     <<"disable_notification">> => undefined | boolean(),
%     <<"protect_content">> => undefined | boolean(),
%     <<"reply_to_message_id">> => undefined | integer(),
%     <<"allow_sending_without_reply">> => undefined | boolean(),
%     <<"reply_markup">> => undefined | keyboard_markup() | force_reply()
% }.

-type message_entity() :: map().
% -type message_entity() :: #{
%     <<"type">> => message_entity_type(),
%     <<"offset">> => integer(),
%     <<"length">> => integer(),
%     <<"url">> => undefined | binary(),
%     <<"user">> => undefined | binary(),
%     <<"language">> => undefined | binary(),
%     <<"custom_emoji_id">> => undefined | binary()
% }.

-type message_entity_type() :: binary().
% -type message_entity_type() :: 
%     <<"mention">>, 
%     <<"hashtag">>, 
%     <<"cashtag">>, 
%     <<"bot_command">>, 
%     <<"url">>, 
%     <<"email">>, 
%     <<"phone_number">>, 
%     <<"bold">>, 
%     <<"italic">>, 
%     <<"underline">>, 
%     <<"strikethrough">>, 
%     <<"spoiler">>, 
%     <<"code">>, 
%     <<"pre">>, 
%     <<"text_link">>, 
%     <<"text_mention">>, 
%     <<"custom_emojis">>.

-type message_id() :: map().
% -type message_id() :: #{
%     <<"message_id">> => integer()
% }.

-type order_info() :: map().
% -type order_info() :: #{
%     <<"id">> => binary(),
%     <<"title">> => binary(),
%     <<"prices">> => [labeled_price()]
% }.

-type photo_size() :: map().
% -type photo_size() :: #{
%     <<"file_id">> => bianry(),
%     <<"file_unique_id">> => binary(),
%     <<"width">> => integer(),
%     <<"height">> => integer(),
%     <<"file_size">> => undefined | integer()
% }.

-type pin_chat_message_optionals() :: map().
% -type pin_chat_message_optionals() :: #{
%     <<"disable_notification">> => undefined | boolean()
% }.

-type poll() :: map().
% -type poll() :: #{
%     <<"id">> => binary(),
%     <<"question">> => binary(),
%     <<"options">> => [poll_option()],
%     <<"total_voter_count">> => integer(),
%     <<"is_closed">> => boolean(),
%     <<"is_anonymous">> => boolean(),
%     <<"type">> => <<"regular">> | <<"quiz">>,
%     <<"allows_multiple_answers">> => boolean(),
%     <<"correct_option_id">> => undefined | integer(),
%     <<"explanation">> => undefined | binary(),
%     <<"explanation_entities">> => undefined | [message_entity()],
%     <<"open_period">> => undefined | integer(),
%     <<"close_date">> => undefined | integer()
% }.

-type poll_answer() :: map().
% -type poll_answer() :: #{
%     <<"poll_id">> => binary(),
%     <<"user">> => user(),
%     <<"option_ids">> => [integer()]
% }.

-type poll_option() :: map().
% -type poll_option() :: #{
%     <<"poll_id">> => binary(),
%     <<"user">> => user(),
%     <<"option_ids">> => [integer()]
% }.

-type pre_checkout_query() :: map().
% -type pre_checkout_query() :: #{
%     <<"id">> => binary(),
%     <<"from">> => user(),
%     <<"currency">> => binary(),
%     <<"total_amount">> => integer(),
%     <<"invoice_payload">> => binary(),
%     <<"shipping_option_id">> => undefined | binary(),
%     <<"order_info">> => undefined | order_info()
% }.

-type promote_chat_member_optionals() :: map().
% -type promote_chat_member_optionals() :: #{
%     <<"is_anonymous">> => undefined | boolean(),
%     <<"can_manage_chat">>  => undefined | boolean(),
%     <<"can_post_mesages">>  => undefined | boolean(),
%     <<"can_edit_messages">>  => undefined | boolean(),
%     <<"can_delete_messages">>  => undefined | boolean(),
%     <<"can_manage_video_chats">>  => undefined | boolean(),
%     <<"can_restrict_members">>  => undefined | boolean(),
%     <<"can_promote_members">>  => undefined | boolean(),
%     <<"can_change_info">>  => undefined | boolean(),
%     <<"can_invite_users">>  => undefined | boolean(),
%     <<"can_pin_messages">>  => undefined | boolean(),
%     <<"can_manage_topics">>  => undefined | boolean()    
% }.

-type reply_keyboard_markup() :: map().
% -type reply_keyboard_markup() :: #{
%     <<"keyboard">> => [[keyboard_button()]],
%     <<"is_persistent">> => undefined | boolean(),
%     <<"resize_keyboard">> => undefined | boolean(),
%     <<"one_time_keyboard">> => undefined | boolean(),
%     <<"input_field_placeholder">> => undefined | binary(),
%     <<"selective">> => undefined | boolean()
% }.

-type reply_keyboard_remove() :: map().
% -type reply_keyboard_remove() :: #{
%     <<"remove_keyboard">> => boolean(),
%     <<"selective">> => undefined | boolean()
% }.

-type response_parameters() :: map().
% -type response_parameters() :: #{
%     <<"migrate_to_chat_id">> => undefined | integer(),
%     <<"retry_after">> => undefined | integer()
% }.

-type restrict_chat_member_optionals() :: map().
% -type restrict_chat_member_optionals() :: #{
%     <<"use_independent_chat_permissions">> => undefined | boolean(),
%     <<"until_date">> => undefined | integer()
% }.

-type send_animation_optionals() :: map().
% -type send_animation_optionals() :: #{
%     <<"message_thread_id">> => undefined | integer(),
%     <<"duration">> => undefined | integer(),
%     <<"width">> => undefined | integer(),
%     <<"height">> => undefined | integer(),
%     <<"thumb">> => undefined | binary() | input_file(),
%     <<"caption">> => undefined | binary(),
%     <<"parse_mode">> => undefined | binary(),
%     <<"caption_entities">> => undefined | [message_entity()],
%     <<"has_spoiler">> => undefined | boolean(),
%     <<"disable_notification">> => undefined | boolean(),
%     <<"protect_content">> => undefined | boolean(),
%     <<"reply_to_message_id">> => undefined | integer(),
%     <<"allow_sending_without_reply">> => undefined | boolean(),
%     <<"reply_markup">> => undefined | keyboard_markup() | force_reply()
% }.

-type send_audio_optionals() :: map().
% -type send_audio_optionals() :: #{
%     <<"message_thread_id">> => undefined | integer(),
%     <<"caption">> => undefined | binary(),
%     <<"parse_mode">> => undefined | binary(),
%     <<"caption_entities">> => undefined | [message_entity()],
%     <<"duration">> => undefined | integer(),
%     <<"performer">> => undefined | binary(),
%     <<"title">> => undefined | binary(),
%     <<"thumb">> => undefined | binary() | input_file(),
%     <<"disable_notification">> => undefined | boolean(),
%     <<"protect_content">> => undefined | boolean(),
%     <<"reply_to_message_id">> => undefined | integer(),
%     <<"allow_sending_without_reply">> => undefined | boolean(),
%     <<"reply_markup">> => undefined | keyboard_markup() | force_reply()
% }.

-type send_chat_action_optionals() :: map().
% -type send_chat_action_optionals() :: #{
%     <<"message_thread_id">> => undefined | integer()
% }.

-type send_contact_optionals() :: map().
% -type send_contact_optionals() :: #{
%     <<"message_thread_id">> => undefined | integer(),
%     <<"last_name">> => undefined | binary(),
%     <<"vcard">> => undefined | binary(),
%     <<"disable_notification">> => undefined | boolean(),
%     <<"protect_content">> => undefined | boolean(),
%     <<"reply_to_message_id">> => undefined | integer(),
%     <<"allow_sending_without_reply">> => undefined | boolean(),
%     <<"reply_markup">> => undefined | keyboard_markup() | force_reply()
% }.

-type send_dice_optionals() :: map().
% -type send_dice_optionals() :: #{
%     <<"message_thread_id">> => undefined | integer(),
%     <<"emoji">> => undefined | binary(),
%     <<"disable_notification">> => undefined | boolean(),
%     <<"protect_content">> => undefined | boolean(),
%     <<"reply_to_message_id">> => undefined | integer(),
%     <<"allow_sending_without_reply">> => undefined | boolean(),
%     <<"reply_markup">> => undefined | keyboard_markup() | force_reply()
% }.

-type send_document_optionals() :: map().
% -type send_document_optionals() :: #{
%     <<"message_thread_id">> => undefined | integer(),
%     <<"thumb">> => undefined | binary() | input_file(),
%     <<"caption">> => undefined | binary(),
%     <<"parse_mode">> => undefined | binary(),
%     <<"caption_entities">> => undefined | [message_entity()],
%     <<"disable_content_type_detection">> => undefined | boolean(),
%     <<"disable_notification">> => undefined | boolean(),
%     <<"protect_content">> => undefined | boolean(),
%     <<"reply_to_message_id">> => undefined | integer(),
%     <<"allow_sending_without_reply">> => undefined | boolean(),
%     <<"reply_markup">> => undefined | keyboard_markup() | force_reply()
% }.

-type send_location_optionals() :: map().
% -type send_location_optionals() :: #{
%     <<"message_thread_id">> => undefined | integer(),
%     <<"horizontal_accuracy">> => undefined | float(),
%     <<"live_period">> => undefined | integer(),
%     <<"heading">> => undefined | integer(),
%     <<"proximity_alert_radius">> => undefined | integer(),
%     <<"disable_notification">> => undefined | boolean(),
%     <<"protect_content">> => undefined | boolean(),
%     <<"reply_to_message_id">> => undefined | integer(),
%     <<"allow_sending_without_reply">> => undefined | boolean(),
%     <<"reply_markup">> => undefined | keyboard_markup() | force_reply()
% }.

-type send_media_group_optionals() :: map().
% -type send_media_group_optionals() :: #{
%     <<"mesage_thread_id">> => undefined | integer(),
%     <<"disable_notification">> => undefined | boolean(),
%     <<"protect_content">> => undefined | boolean(),
%     <<"reply_to_message_id">> => undefined | integer(),
%     <<"allow_sending_without_reply">> => undefined | boolean()
% }.

-type send_message_optionals() :: map().
% -type send_message_optionals() :: #{
%     <<"message_thread_id">> => undefined | integer(),
%     <<"parse_mode">> => undefined | 'MarkdownV2' | 'HTML',
%     <<"entities">> => undefined | [message_entity()],
%     <<"disable_web_page_preview">> => undefined | boolean(),
%     <<"disable_notification">> => undefined | boolean(),
%     <<"protect_content">> => undefined | boolean(),
%     <<"reply_to_message_id">> => undefined | boolean(),
%     <<"allow_sending_without_reply">> => undefined | boolean(),
%     <<"reply_markup">> => undefined | keyboard_markup() | force_reply()
% }.

-type send_photo_optionals() :: map().
% -type send_photo_optionals() :: #{
%     <<"message_thread_id">> => undefined | integer(),
%     <<"caption">> => undefined | binary(),
%     <<"parse_mode">> => undefined | binary(),
%     <<"caption_entities">> => undefined | [message_entity()],
%     <<"has_spoiler">> => undefined | boolean(),
%     <<"disable_notification">> => undefined | boolean(),
%     <<"protect_content">> => undefined | boolean(),
%     <<"reply_to_message_id">> => undefined | integer(),
%     <<"allow_sending_without_reply">> => undefined | boolean(),
%     <<"reply_markup">> => undefined | keyboard_markup() | force_reply()
% }.

-type send_poll_optionals() :: map().
% -type send_poll_optionals() :: #{
%     <<"message_thread_id">> => undefined | integer(),
%     <<"is_anonymous">> => undefined | boolean(),
%     <<"type">> => undefined | binary(),
%     <<"allows_multiple_answers">> => undefined | boolean(),
%     <<"correct_option_id">> => undefined | integer(),
%     <<"explanation">> => undefined | binary(),
%     <<"explanation_parse_mode">> => undefined | binary(),
%     <<"explanation_entities">> => undefined | [message_entity()],
%     <<"open_period">> => undefined | integer(),
%     <<"close_date">> => undefined | integer(),
%     <<"is_closed">> => undefined | boolean(),
%     <<"disable_notification">> => undefined | boolean(),
%     <<"protect_content">> => undefined | booelan(),
%     <<"reply_to_message_id">> => undefined | integert(),
%     <<"allow_sending_without_reply">> => undefined | boolean(),
%     <<"reply_markup">> => undefined | keyboard_markup() | force_reply()
% }.

-type send_venue_optionals() :: map().
% -type send_venue_optionals() :: #{
%     <<"message_thread_id">> => undefined | integer(),
%     <<"foursquare_id">> => undefined | binary(),
%     <<"foursquare_type">> => undefined | binary(),
%     <<"google_place_id">> => undefined | bianry(),
%     <<"google_place_type">> => undefined | binary(),
%     <<"disable_notification">> => undefined | boolean(),
%     <<"protect_content">> => undefined | boolean(),
%     <<"reply_to_message_id">> => undefined | integer(),
%     <<"allow_sending_without_reply">> => undefined | boolean(),
%     <<"reply_markup">> => undefined | keyboard_markup() | force_reply().
% }.

-type send_video_optionals() :: map().
% -type send_video_optionals() :: #{
%     <<"message_thread_id">> => undefined | integer(),
%     <<"duration">> => undefined | integer(),
%     <<"width">> => undefined | integer(),
%     <<"height">> => undefined | integer(),
%     <<"thumb">> => undefined | binary() | input_file(),
%     <<"caption">> => undefined | binary(),
%     <<"parse_mode">> => undefined | binary(),
%     <<"caption_entities">> => undefined | [message_entity()],
%     <<"has_spoiler">> => undefined | boolean(),
%     <<"supports_streaming">> => undefined | boolean(),
%     <<"disable_notification">> => undefined | boolean(),
%     <<"protect_content">> => undefined | boolean(),
%     <<"reply_to_message_id">> => undefined | integer(),
%     <<"allow_sending_without_reply">> => undefined | boolean(),
%     <<"reply_markup">> => undefined | keyboard_markup() | force_reply()
% }.

-type send_video_note_optionals() :: map().
% -type send_video_note_optionals() :: #{
%     <<"message_thread_id">> => undefined | integer(),
%     <<"duration">> => undefined | integer(),
%     <<"length">> => undefined | integer(),
%     <<"thumb">> => undefined | binary() | input_file(),
%     <<"disable_notification">> => undefined | boolean(),
%     <<"protect_content">> => undefined | boolean(),
%     <<"reply_to_message_id">> => undefined | integer(),
%     <<"allow_sending_without_reply">> => undefined | boolean(),
%     <<"reply_markup">> => undefined | keyboard_markup() | force_reply()
% }.

-type send_voice_optionals() :: map().
% -type send_voice_optionals() :: #{
%     <<"message_thread_id">> => undefined | integer(),
%     <<"caption">> => undefined | binary(),
%     <<"parse_mode">> => undefined | binary(),
%     <<"caption_entities">> => undefined | [message_entity()],
%     <<"duration">> => undefined | integer(),
%     <<"disable_notification">> => undefined | boolean(),
%     <<"protect_content">> => undefined | boolean(),
%     <<"reply_to_message_id">> => undefined | integer(),
%     <<"allow_sending_without_reply">> => undefined | boolean(),
%     <<"reply_markup">> => undefined | keyboard_markup() | force_reply()
% }.

-type set_chat_description_optionals() :: map().
% -type set_chat_description_optionals() :: #{
%     <<"description">> => undefined | binary()
% }.

-type set_chat_permissions_optionals() :: map().
% -type set_chat_permissions_optionals() :: #{
%     <<"use_independent_chat_permissions">> => undefined | boolean()
% }.

-type set_my_commands_optionals() :: map().
% -type set_my_commands_optionals() :: #{
%     <<"scope">> => undefined | bot_command_scope(),
%     <<"language_code">> => undefined | binary()
% }.

-type set_my_default_administrator_rights_optionals() :: map().
% -type set_my_default_administrator_rights_optionals() :: #{
%     <<"rights">> => undefined | chat_administrator_rights(),
%     <<"for_channels">> => undefined | boolean()
% }.

-type shipping_address() :: map().
% -type shipping_address() :: #{
%     <<"country_code">> => binary(),
%     <<"state">> => binary(),
%     <<"city">> => binary(),
%     <<"street_line1">> => binary(),
%     <<"street_line2">> => binary(),
%     <<"post_code">> => binary()
% }.

-type shipping_query() :: map().
% -type shipping_query() :: #{
%     <<"id">> => binary(),
%     <<"from">> => user(),
%     <<"invoice_payload">> => binary(),
%     <<"shipping_address">> => shipping_address()
% }.

-type sticker() :: map().
% -type sticker() :: #{
%     <<"field_id">> => binary(),
%     <<"file_unique_id">> => binary(),
%     <<"type">> => binary(),
%     <<"width">> => integer(),
%     <<"height">> => integer(),
%     <<"is_animated">> => boolean(),
%     <<"is_video">> => boolean(),
%     <<"thumb">> => undefined | photo_size(),
%     <<"emoji">> => undefined | bianry(),
%     <<"set_name">> => undefined | binary(),
%     <<"premium_animation">> => undefined | file(),
%     <<"mask_position">> => undefined | mask_position(),
%     <<"custom_emoji_id">> => undefined | binary(),
%     <<"file_size">> => undefined | integer()
% }.

-type stop_message_live_location_optionals() :: map().
% -type stop_message_live_location_optionals() :: #{
%     <<"chat_id">> => undefined | integer() | binary(),
%     <<"message_id">> => undefined | integer(),
%     <<"inline_message_id">> => undefined | binary(),
%     <<"reply_markup">> => undefined | inline_keyboard_markup()
% }.

-type tbc_response(Type) :: {ok, api_response(Type)} | {error, any()} | not_implemented_yet.

-type token() :: [0..255].

-type unban_chat_member_optionals() :: map().
% -type unban_chat_member_optionals() :: #{
%     <<"only_if_banned">> => undefined | boolean()
% }.

-type unpin_chat_message_optionals() :: map().
% -type unpin_chat_message_optionals() :: #{
%     <<"message_id">> => undefined | integer()
% }.

-type update() :: map().
% -type update() :: #{
%     <<"update_id">> => integer(),
%     <<"message">> => undefined | message(),
%     <<"edited_message">> => undefined | message(),
%     <<"channel_post">> => undefined | message(),
%     <<"edited_channel_post">> => undefined | message(),
%     <<"inline_query">> => undefined | inline_query(),
%     <<"chosen_inline_result">> => undefined | chosen_inline_result(),
%     <<"callback_query">> => undefined | callback_query(),
%     <<"shipping_query">> => undefined | shippipng_query(),
%     <<"pre_checkout_query">> => undefined | pre_checkout_query(),
%     <<"poll">> => undefined | poll(),
%     <<"poll_answer">> => undefined | poll_answer(),
%     <<"my_chat_member">> => undefined | chat_member_updated(),
%     <<"chat_member">> => undefined | chat_member_updated(),
%     <<"chat_join_request">> => undefined | chat_join_request()
% }.

-type user() :: map().
% -type user() :: #{
%   <<"id">> => user_id(),
%   <<"is_bot">> => boolean(),
%   <<"first_name">> => binary(),
%   <<"last_name">> => undefined | binary(),
%   <<"username">> => undefined | binary(),
%   <<"language_code">> => undefined | binary(),
%   <<"is_premium">> => undefined | boolean(),
%   <<"added_to_attachment_menu">> => undefined | boolean()
%   <<"can_join_groups">> => undefined | boolean()
%   <<"can_read_all_group_messages">> => undefined | boolean()
%   <<"supports_inline_queries">> => undefined | boolean()
% }.

-type user_id() :: integer().

-type user_profile_photos() :: map().
% -type user_profile_photos() :: #{
%     <<"total_count">> => integer(),
%     <<"photos">> => [[photo_size()]]
% }.

-type web_app_info() :: map().
% -type web_app_info() :: #{
%     <<"url">> => binary()
% }.

-endif.
