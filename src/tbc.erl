-module(tbc).

-include("tbc.hrl").

%%% PUBLIC API
-export([
    get_me/1,
    log_out/1,
    close/1,
    send_message/4,
    forward_message/5,
    copy_message/5,
    send_photo/4,
    send_audio/4,
    send_document/4,
    send_video/4,
    send_animation/4,
    send_voice/4,
    send_video_note/4,
    send_media_group/4,
    send_location/5,
    edit_message_live_location/4,
    stop_message_live_location/2,
    send_venue/7,
    send_contact/5,
    send_poll/5,
    send_dice/3,
    send_chat_action/4,
    get_user_profile_photos/3,
    get_file/2,
    ban_chat_member/4,
    unban_chat_member/4,
    restrict_chat_member/5,
    promote_chat_member/4,
    set_chat_administrator_custom_title/4,
    ban_chat_sender_chat/3,
    unban_chat_sender_chat/3,
    set_chat_permissions/4,
    export_chat_invite_link/2,
    create_chat_invite_link/3,
    edit_chat_invite_link/4,
    revoke_chat_invite_link/3,
    approve_chat_join_request/3,
    decline_chat_join_request/3,
    set_chat_photo/3,
    delete_chat_photo/2,
    set_chat_title/3,
    set_chat_description/3,
    pin_chat_message/4,
    unpin_chat_message/3,
    unpin_all_chat_message/2,
    leave_chat/2,
    get_chat/2,
    get_chat_administrators/2,
    get_chat_member_count/2,
    get_chat_member/3,
    get_updates/2
]).

%%% ---------------------------------------------
%%% PUBLIC API
%%% ---------------------------------------------

% TODO: custom error responses to make expicit
% the reason causing `false`s as response.

-spec get_me(Token) -> Resp when
    Token :: binary(),
    Resp :: user().
get_me(Token) ->
    Params = #{},
    send(Token, Params).

-spec log_out(Token) -> Resp when
    Token :: binary(),
    Resp :: boolean().
log_out(Token) ->
    not_implemented_yet.

-spec close(Token) -> Resp when
    Token :: binary(),
    Resp :: boolean(). 
close(Token) ->
    not_implemented_yet.
    
-spec send_message(Token, ChatId, Text, Optionals) -> Resp when
    Token :: binary(),
    ChatId :: chat_id(),
    Text :: binary(),
    Optionals :: send_message_optionals(),
    Resp :: message().
send_message(Token, ChatId, Text, Optionals) ->
    not_implemented_yet.

-spec forward_message(Token, ChatId, FromChatId, MessageId, Optionals) -> Resp when
    Token :: binary(),
    ChatId :: chat_id(),
    FromChatId :: chat_id(),
    MessageId :: integer(),
    Optionals :: forward_message_optionals(),
    Resp :: boolean().
forward_message(Token, ChatId, FromChatId, MessageId, Optionals) ->
    not_implemented_yet.

-spec copy_message(Token, ChatId, FromChatId, MessageId, Optionals) -> Resp when
    Token :: binary(),
    ChatId :: binary(),
    FromChatId :: chat_id(),
    MessageId :: integer(),
    Optionals :: copy_message_optionals(),
    Resp :: message_id().

copy_message(Token, ChatId, FromChatId, MessageId, Optiona) -> 
    not_implemented_yet.

-spec send_photo(Token, ChatId, Photo, Optionals) -> Resp when
    Token :: binary(),
    ChatId :: chat_id(),
    Photo :: binary() | input_file(),
    Optionals :: send_photo_optionals(),
    Resp :: message().
send_photo(Token, ChatId, Photo, Optionals) -> 
    not_implemented_yet.

-spec send_audio(Token, ChatId, Audio, Optionals) -> Resp when
    Token :: binary(),
    ChatId :: chat_id(),
    Audio :: binary() | input_file(),
    Optionals :: send_audio_optionals(),
    Resp :: message().
send_audio(Token, ChatId, Audio, Optionals) ->
    not_implemented_yet.

-spec send_document(Token, ChatId, Document, Optionals) -> Resp when
    Token :: binary(),
    ChatId :: chat_id(),
    Document :: binary() | input_file(),
    Optionals :: send_document_optionals(),
    Resp :: message().
send_document(Token, ChatId, Document, Optionals) ->
    not_implemented_yet.

-spec send_video(Token, ChatId, Video, Optionals) -> Resp when
    Token :: binary(),
    ChatId :: chat_id(),
    Video :: binary(),
    Optionals :: send_video_optionals(),
    Resp :: message().
send_video(Token, ChatId, Video, Optionals) ->
    not_implemented_yet.

-spec send_animation(Token, ChatId, Animation, Optionals) -> Resp when
    Token :: binary(),
    ChatId :: chat_id(),
    Animation :: binary() | input_file(),
    Optionals :: send_animation_optionals(),
    Resp :: message().
send_animation(Token, ChatId, Animation, Optionals) ->
    not_implemented_yet.

-spec send_voice(Token, ChatId, Voice, Optionals) -> Resp when
    Token :: binary(),
    ChatId :: chat_id(),
    Voice :: binary() | input_file(),
    Optionals :: send_voice_optionals(),
    Resp :: message().
send_voice(Token, ChatId, Voice, Optionals) ->
    not_implemented_yet.

-spec send_video_note(Token, ChatId, VideoNote, Optionals) -> Resp when
    Token :: binary(),
    ChatId :: chat_id(),
    VideoNote :: binary() | input_file(),
    Optionals :: send_video_note_optionals(),
    Resp :: message().
send_video_note(Token, ChatId, VideoNote, Optionals) ->
    not_implemented_yet.

-spec send_media_group(Token, ChatId, Media, Optionals) -> Resp when
    Token :: binary(),
    ChatId :: chat_id(),
    Media :: [input_media()],
    Optionals :: send_media_group_optionals(),
    Resp :: message().
send_media_group(Token, ChatId, Media, Optionals) ->
    not_implemented_yet.

-spec send_location(Token, ChatId, Latitude, Longitude, Optionals) -> Resp when
    Token :: binary(),
    ChatId :: chat_id(),
    Latitude :: float(),
    Longitude :: float(),
    Optionals :: send_location_optionals(),
    Resp :: message().
send_location(Token, ChatId, Latitude, Longitude, Optionals) -> 
    not_implemented_yet.

-spec edit_message_live_location(Token, Latitude, Longitude, Optionals) -> Resp when
    Token :: binary(),
    Latitude :: float(),
    Longitude :: float(),
    Optionals :: edit_message_live_location_optionals(),
    Resp :: true | message().
edit_message_live_location(Token, Latitude, Longitude, Optionals) ->
    not_implemented_yet.

-spec stop_message_live_location(Token, Optionals) -> Resp when
    Token :: binary(),
    Optionals :: stop_message_live_location_optionals(),
    Resp :: true | message().
stop_message_live_location(Token, Optionals) ->
    not_implemented_yet.

-spec send_venue(Token, ChatId, Latitude, Longitude, Title, Address, Optionals) -> Resp when
    Token :: binary(),
    ChatId :: chat_id(),
    Latitude :: float(),
    Longitude :: float(),
    Title :: binary(),
    Address :: binary(),
    Optionals :: send_venue_optionals(),
    Resp :: message().
send_venue(Token, ChatId, Latitude, Longitude, Title, Address, Optionals) ->
    not_implemented_yet.

-spec send_contact(Token, ChatId, PhoneNumber, FirstName, Optionals) -> Resp when
    Token :: binary(),
    ChatId :: chat_id(),
    PhoneNumber :: binary(),
    FirstName :: binary(),
    Optionals :: send_contact_optionals(),
    Resp :: message().
send_contact(Token, ChatId, PhoneNumber, FirstName, Optionals) ->
    not_implemented_yet.    

-spec send_poll(Token, ChatId, Question, Options, Optionals) -> Resp when
    Token :: binary(),
    ChatId :: chat_id(),
    Question :: binary(),
    Options :: [binary()],
    Optionals :: send_poll_optionals(),
    Resp :: message().
send_poll(Token, ChatId, Question, Options, Optionals) -> 
    not_implemented_yet.

-spec send_dice(Token, ChatId, Optionals) -> Resp when
    Token :: binary(),
    ChatId :: chat_id(),
    Optionals :: send_dice_optionals(),
    Resp :: message().
send_dice(Token, ChatId, Optionals) ->
    not_implemented_yet.

-spec send_chat_action(Token, ChatId, Action, Optionals) -> Resp when
    Token :: binary(),
    ChatId :: chat_id(),
    Action :: binary(),
    Optionals :: send_chat_action_optionals(),
    Resp :: true.
send_chat_action(Token, ChatId, Action, Optionals) ->
    not_implemented_yet.

-spec get_user_profile_photos(Token, UserId, Optionals) -> Resp when
    Token :: binary(),
    UserId :: user_id(),
    Optionals :: get_user_profile_photos_optionals(),
    Resp :: user_profile_photos().
get_user_profile_photos(Token, UserId, Optionals) ->
    not_implemented_yet.

-spec get_file(Token, FileId) -> Resp when
    Token :: binary(),
    FileId :: binary(),
    Resp :: file().
get_file(Token, FileId) ->
    not_implemented_yet.

-spec ban_chat_member(Token, ChatId, UserId, Optionals) -> Resp when
    Token :: binary(),
    ChatId :: chat_id(),
    UserId :: user_id(),
    Optionals :: ban_chat_member_optionals(),
    Resp :: true.
ban_chat_member(Token, ChatId, UserId, Optionals) ->
    not_implemented_yet.

-spec unban_chat_member(Token, ChatId, UserId, Optionals) -> Resp when
    Token :: binary(),
    ChatId :: chat_id(),
    UserId :: user_id(),
    Optionals :: unban_chat_member_optionals(),
    Resp :: true.
unban_chat_member(Token, ChatId, UserId, Optionals) ->
    not_implemented_yet.

-spec restrict_chat_member(Token, ChatId, UserId, Permissions, Optionals) -> Resp when
    Token :: binary(),
    ChatId :: chat_id(),
    UserId :: user_id(),
    Permissions :: chat_permissions(),
    Optionals :: restrict_chat_member_optionals(),
    Resp :: true.
restrict_chat_member(Token, ChatId, UserId, Permissions, Optionals) ->
    not_implemented_yet.

-spec promote_chat_member(Token, ChatId, UserId, Optionals) -> Resp when
    Token :: binary(),
    ChatId :: chat_id(),
    UserId :: user_id(),
    Optionals :: promote_chat_member_optionals(),
    Resp :: true.
promote_chat_member(Token, ChatId, UserId, Optionals) ->
    not_implemented_yet.

-spec set_chat_administrator_custom_title(Token, ChatId, UserId, CustomTitle) -> Resp when
    Token :: binary(),
    ChatId :: chat_id(),
    UserId :: user_id(),
    CustomTitle :: binary(),
    Resp :: true.
set_chat_administrator_custom_title(Token, ChatId, UserId, CustomTitle) ->
    not_implemented_yet.

-spec ban_chat_sender_chat(Token, ChatId, SenderChatId) -> Resp when
    Token :: binary(),
    ChatId :: chat_id(),
    SenderChatId :: integer(),
    Resp :: true.
ban_chat_sender_chat(Token, ChatId, SenderChatId) ->
    not_implemented_yet.

-spec unban_chat_sender_chat(Token, ChatId, SenderChatId) -> Resp when
    Token :: binary(),
    ChatId :: chat_id(),
    SenderChatId :: integer(),
    Resp :: true.
unban_chat_sender_chat(Token, ChatId, SenderChatId) ->
    not_implemented_yet.

-spec set_chat_permissions(Token, ChatId, Permissions, Optionals) -> Resp when
    Token :: binary(),
    ChatId :: chat_id(),
    Permissions :: chat_permissions(),
    Optionals :: set_chat_permissions_optionals(),
    Resp :: true.
set_chat_permissions(Token, ChatId, Permissions, Optionals) ->
    not_implemented_yet.

-spec export_chat_invite_link(Token, ChatId) -> Resp when
    Token :: binary(),
    ChatId :: chat_id(),
    Resp :: binary().
export_chat_invite_link(Token, ChatId) ->
    not_implemented_yet.


-spec create_chat_invite_link(Token, ChatId, Optionals) -> Resp when
    Token :: binary(),
    ChatId :: chat_id(),
    Optionals :: create_chat_invite_link_optionals(),
    Resp :: chat_invite_link().
create_chat_invite_link(Token, ChatId, Optionals) ->
    not_implemented_yet.

-spec edit_chat_invite_link(Token, ChatId, InviteLink, Optionals) -> Resp when
    Token :: binary(),
    ChatId :: chat_id(),
    InviteLink :: binary(),
    Optionals :: create_chat_invite_link_optionals(),
    Resp :: chat_invite_link().
edit_chat_invite_link(Token, ChatId, InviteLink, Optionals) ->
    not_implemented_yet.

-spec revoke_chat_invite_link(Token, ChatId, InviteLink) -> Resp when
    Token :: binary(),
    ChatId :: chat_id(),
    InviteLink :: binary(),
    Resp :: chat_invite_link().
revoke_chat_invite_link(Token, ChatId, InviteLink) ->
    not_implemented_yet.

-spec approve_chat_join_request(Token, ChatId, UserId) -> Resp when
    Token :: binary(),
    ChatId :: chat_id(),
    UserId :: user_id(),
    Resp :: true.
approve_chat_join_request(Token, ChatId, UserId) ->
    not_implemented_yet.

-spec decline_chat_join_request(Token, ChatId, UserId) -> Resp when
    Token :: binary(),
    ChatId :: chat_id(),
    UserId :: user_id(),
    Resp :: true.
decline_chat_join_request(Token, ChatId, UserId) ->
    not_implemented_yet.

-spec set_chat_photo(Token, ChatId, Photo) -> Resp when
    Token :: binary(),
    ChatId :: chat_id(),
    Photo :: input_file(),
    Resp :: true.
set_chat_photo(Token, ChatId, Photo) ->
    not_implemented_yet.

-spec delete_chat_photo(Token, ChatId) -> Resp when
    Token :: binary(),
    ChatId :: chat_id(),
    Resp :: true.
delete_chat_photo(Token, ChatId) ->
    not_implemented_yet.

-spec set_chat_title(Token, ChatId, Title) -> Resp when
    Token :: binary(),
    ChatId :: chat_id(),
    Title :: binary(),
    Resp :: true.
set_chat_title(Token, ChatId, Title) ->
    not_implemented_yet.

-spec set_chat_description(Token, ChatId, Optionals) -> Resp when
    Token :: binary(),
    ChatId :: chat_id(),
    Optionals :: set_chat_description_optionals(),
    Resp :: true.
set_chat_description(Token, ChatId, Optionals) ->
    not_implemented_yet.

-spec pin_chat_message(Token, ChatId, MessageId, Optionals) -> Resp when
    Token :: binary(),
    ChatId :: chat_id(),
    MessageId :: integer(),
    Optionals :: pin_chat_message_optionals(),
    Resp :: true.
pin_chat_message(Token, ChatId, MessageId, Optionals) ->
    not_implemented_yet.

-spec unpin_chat_message(Token, ChatId, Optionals) -> Resp when
    Token :: binary(),
    ChatId :: chat_id(),
    Optionals :: unpin_chat_message_optionals(),
    Resp :: true.
unpin_chat_message(Token, ChatId, Optionals) ->
    not_implemented_yet.

-spec unpin_all_chat_message(Token, ChatId) -> Resp when
    Token :: binary(),
    ChatId :: chat_id(),
    Resp :: true.
unpin_all_chat_message(Token, ChatId) ->
    not_implemented_yet.

-spec leave_chat(Token, ChatId) -> Resp when
    Token :: binary(),
    ChatId :: chat_id(),
    Resp :: true.
leave_chat(Token, ChatId) ->
    not_implemented_yet.

-spec get_chat(Token, ChatId) -> Resp when
    Token :: binary(),
    ChatId :: chat_id(),
    Resp :: chat().
get_chat(Token, ChatId) ->
    not_implemented_yet.

-spec get_chat_administrators(Token, ChatId) -> Resp when
    Token :: binary(),
    ChatId :: chat_id(),
    Resp :: [chat_member()].
get_chat_administrators(Token, ChatId) ->
    not_implemented_yet.

-spec get_chat_member_count(Token, ChatId) -> Resp when
    Token :: binary(),
    ChatId :: chat_id(),
    Resp :: integer().
get_chat_member_count(Token, ChatId) ->
    not_implemented_yet.

-spec get_chat_member(Token, ChatId, UserId) -> Resp when
    Token :: binary(),
    ChatId :: chat_id(),
    UserId :: user_id(),
    Resp :: chat_member().
get_chat_member(Token, ChatId, UserId) ->
    not_implemented_yet.

-spec get_updates(Token, Optional) -> Resp when
    Token :: binary(),
    Optional :: get_updates_optionals(),
    Resp :: [update()].
get_updates(Token, Optional) ->
    not_implemented_yet.


%%% ---------------------------------------------
%%% INTERNAL FUNCTIONS
%%% ---------------------------------------------
send(Token, Params) ->
    ok.