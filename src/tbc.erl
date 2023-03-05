-module(tbc).

-include("tbc.hrl").

-include_lib("kernel/include/logger.hrl").

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
    set_chat_sticker_set/3,
    delete_chat_sticker_set/2,
    get_forum_topic_icon_stickers/1,
    create_forum_topic/4,
    edit_forum_topic/4,
    close_forum_topic/3,
    reopen_forum_topic/3,
    delete_forum_topic/3,
    unpin_all_forum_topic_messages/3,
    edit_general_forum_topic/3,
    close_general_forum_topic/2,
    reopen_general_forum_topic/2,
    hide_general_forum_topic/2,
    unhide_general_forum_topic/2,
    answer_callback_query/3,
    set_my_commands/3,
    delete_my_commands/2,
    get_my_commands/2,
    get_chat_menu_button/2,
    set_my_default_administrator_rights/2,
    get_my_default_administrator_rights/2,
    get_updates/2
]).

%%% ---------------------------------------------
%%% PUBLIC API
%%% ---------------------------------------------
-spec get_me(Token) -> Resp when
    Token :: token(),
    Resp :: tbc_response(user()).
get_me(Token) ->
    Params = #{},
    send(Token, "getMe", Params).

-spec log_out(Token) -> Resp when
    Token :: token(),
    Resp :: tbc_response(boolean()).
log_out(_Token) ->
    not_implemented_yet.

-spec close(Token) -> Resp when
    Token :: token(),
    Resp :: tbc_response(boolean()). 
close(_Token) ->
    not_implemented_yet.
    
-spec send_message(Token, ChatId, Text, Optionals) -> Resp when
    Token :: token(),
    ChatId :: chat_id(),
    Text :: binary(),
    Optionals :: send_message_optionals(),
    Resp :: tbc_response(message()).
send_message(Token, ChatId, Text, Optionals) when is_binary(Text) ->
    Params0 = #{
        <<"chat_id">> => ChatId,
        <<"text">> => Text
    },
    Params = maps:merge(Params0, Optionals),
    send(Token, "sendMessage", Params).

-spec forward_message(Token, ChatId, FromChatId, MessageId, Optionals) -> Resp when
    Token :: token(),
    ChatId :: chat_id(),
    FromChatId :: chat_id(),
    MessageId :: integer(),
    Optionals :: forward_message_optionals(),
    Resp :: tbc_response(message()).
forward_message(Token, ChatId, FromChatId, MessageId, Optionals) ->
    Params0 = #{
        <<"chat_id">> => ChatId,
        <<"from_chat_id">> => FromChatId,
        <<"message_id">> => MessageId
    },
    Params = maps:merge(Params0, Optionals),
    send(Token, "forwardMessage", Params).

-spec copy_message(Token, ChatId, FromChatId, MessageId, Optionals) -> Resp when
    Token :: token(),
    ChatId :: binary(),
    FromChatId :: chat_id(),
    MessageId :: integer(),
    Optionals :: copy_message_optionals(),
    Resp :: tbc_response(message_id()).

copy_message(Token, ChatId, FromChatId, MessageId, Optionals) -> 
    Params0 = #{
        <<"chat_id">> => ChatId,
        <<"from_chat_id">> => FromChatId,
        <<"message_id">> => MessageId
    },
    Params = maps:merge(Params0, Optionals),
    send(Token, "copyMessage", Params).

-spec send_photo(Token, ChatId, Photo, Optionals) -> Resp when
    Token :: token(),
    ChatId :: chat_id(),
    Photo :: binary() | input_file(),
    Optionals :: send_photo_optionals(),
    Resp :: tbc_response(message()).
send_photo(Token, ChatId, Photo, Optionals) -> 
    Params0 = #{
        <<"chat_id">> => ChatId,
        <<"photo">> => Photo
    },
    Params = maps:merge(Params0, Optionals),
    send(Token, "sendPhoto", Params).

-spec send_audio(Token, ChatId, Audio, Optionals) -> Resp when
    Token :: token(),
    ChatId :: chat_id(),
    Audio :: binary() | input_file(),
    Optionals :: send_audio_optionals(),
    Resp :: tbc_response(message()).
send_audio(Token, ChatId, Audio, Optionals) ->
    Params0 = #{
        <<"chat_id">> => ChatId,
        <<"audio">> => Audio
    },
    Params = maps:merge(Params0, Optionals),
    send(Token, "sendAudio", Params).

-spec send_document(Token, ChatId, Document, Optionals) -> Resp when
    Token :: token(),
    ChatId :: chat_id(),
    Document :: binary() | input_file(),
    Optionals :: send_document_optionals(),
    Resp :: tbc_response(message()).
send_document(Token, ChatId, Document, Optionals) ->
    Params0 = #{
        <<"chat_id">> => ChatId,
        <<"document">> => Document
    },
    Params = maps:merge(Params0, Optionals),
    send(Token, "sendDocument", Params).

-spec send_video(Token, ChatId, Video, Optionals) -> Resp when
    Token :: token(),
    ChatId :: chat_id(),
    Video :: binary(),
    Optionals :: send_video_optionals(),
    Resp :: tbc_response(message()).
send_video(Token, ChatId, Video, Optionals) ->
    Params0 = #{
        <<"chat_id">> => ChatId,
        <<"video">> => Video
    },
    Params = maps:merge(Params0, Optionals),
    send(Token, "sendVideo", Params).

-spec send_animation(Token, ChatId, Animation, Optionals) -> Resp when
    Token :: token(),
    ChatId :: chat_id(),
    Animation :: binary() | input_file(),
    Optionals :: send_animation_optionals(),
    Resp :: tbc_response(message()).
send_animation(Token, ChatId, Animation, Optionals) ->
    Params0 = #{
        <<"chat_id">> => ChatId,
        <<"animation">> => Animation
    },
    Params = maps:merge(Params0, Optionals),
    send(Token, "sendAnimation", Params).

-spec send_voice(Token, ChatId, Voice, Optionals) -> Resp when
    Token :: token(),
    ChatId :: chat_id(),
    Voice :: binary() | input_file(),
    Optionals :: send_voice_optionals(),
    Resp :: tbc_response(message()).
send_voice(Token, ChatId, Voice, Optionals) ->
    Params0 = #{
        <<"chat_id">> => ChatId,
        <<"voice">> => Voice
    },
    Params = maps:merge(Params0, Optionals),
    send(Token, "sendVoice", Params).

-spec send_video_note(Token, ChatId, VideoNote, Optionals) -> Resp when
    Token :: token(),
    ChatId :: chat_id(),
    VideoNote :: binary() | input_file(),
    Optionals :: send_video_note_optionals(),
    Resp :: tbc_response(message()).
send_video_note(Token, ChatId, VideoNote, Optionals) ->
    Params0 = #{
        <<"chat_id">> => ChatId,
        <<"video_note">> => VideoNote
    },
    Params = maps:merge(Params0, Optionals),
    send(Token, "sendVideoNote", Params).

-spec send_media_group(Token, ChatId, Media, Optionals) -> Resp when
    Token :: token(),
    ChatId :: chat_id(),
    Media :: [input_media()],
    Optionals :: send_media_group_optionals(),
    Resp :: tbc_response(message()).
send_media_group(Token, ChatId, Media, Optionals) ->
    Params0 = #{
        <<"chat_id">> => ChatId,
        <<"media">> => Media
    },
    Params = maps:merge(Params0, Optionals),
    send(Token, "sendMediaGroup", Params).

-spec send_location(Token, ChatId, Latitude, Longitude, Optionals) -> Resp when
    Token :: token(),
    ChatId :: chat_id(),
    Latitude :: float(),
    Longitude :: float(),
    Optionals :: send_location_optionals(),
    Resp :: tbc_response(message()).
send_location(Token, ChatId, Latitude, Longitude, Optionals) -> 
    Params0 = #{
        <<"chat_id">> => ChatId,
        <<"latitude">> => Latitude,
        <<"longitude">> => Longitude
    },
    Params = maps:merge(Params0, Optionals),
    send(Token, "sendLocation", Params).

-spec edit_message_live_location(Token, Latitude, Longitude, Optionals) -> Resp when
    Token :: token(),
    Latitude :: float(),
    Longitude :: float(),
    Optionals :: edit_message_live_location_optionals(),
    Resp :: true | tbc_response(message()).
edit_message_live_location(Token, Latitude, Longitude, Optionals) ->
    Params0 = #{
        <<"latitude">> => Latitude,
        <<"longitude">> => Longitude
    },
    Params = maps:merge(Params0, Optionals),
    send(Token, "editMessageLiveAction", Params).

-spec stop_message_live_location(Token, Optionals) -> Resp when
    Token :: token(),
    Optionals :: stop_message_live_location_optionals(),
    Resp :: true | tbc_response(message()).
stop_message_live_location(Token, Optionals) ->
    send(Token, "stopMessageLiveLocation", Optionals).

-spec send_venue(Token, ChatId, Latitude, Longitude, Title, Address, Optionals) -> Resp when
    Token :: token(),
    ChatId :: chat_id(),
    Latitude :: float(),
    Longitude :: float(),
    Title :: binary(),
    Address :: binary(),
    Optionals :: send_venue_optionals(),
    Resp :: tbc_response(message()).
send_venue(Token, ChatId, Latitude, Longitude, Title, Address, Optionals) ->
    Params0 = #{
        <<"chat_id">> => ChatId,
        <<"latitude">> => Latitude,
        <<"longitude">> => Longitude,
        <<"title">> => Title,
        <<"address">> => Address
    },
    Params = maps:merge(Params0, Optionals),
    send(Token, "sendVenue", Params).

-spec send_contact(Token, ChatId, PhoneNumber, FirstName, Optionals) -> Resp when
    Token :: token(),
    ChatId :: chat_id(),
    PhoneNumber :: binary(),
    FirstName :: binary(),
    Optionals :: send_contact_optionals(),
    Resp :: tbc_response(message()).
send_contact(Token, ChatId, PhoneNumber, FirstName, Optionals) ->
    Params0 = #{
        <<"chat_id">> => ChatId,
        <<"phone_number">> => PhoneNumber,
        <<"first_name">> => FirstName
    },
    Params = maps:merge(Params0, Optionals),
    send(Token, "sendContact", Params).  

-spec send_poll(Token, ChatId, Question, Options, Optionals) -> Resp when
    Token :: token(),
    ChatId :: chat_id(),
    Question :: binary(),
    Options :: [binary()],
    Optionals :: send_poll_optionals(),
    Resp :: tbc_response(message()).
send_poll(Token, ChatId, Question, Options, Optionals) -> 
    Params0 = #{
        <<"chat_id">> => ChatId,
        <<"question">> => Question,
        <<"options">> => Options
    },
    Params = maps:merge(Params0, Optionals),
    send(Token, "sendPoll", Params).  

-spec send_dice(Token, ChatId, Optionals) -> Resp when
    Token :: token(),
    ChatId :: chat_id(),
    Optionals :: send_dice_optionals(),
    Resp :: tbc_response(message()).
send_dice(Token, ChatId, Optionals) ->
    Params0 = #{
        <<"chat_id">> => ChatId
    },
    Params = maps:merge(Params0, Optionals),
    send(Token, "sendDice", Params).  

-spec send_chat_action(Token, ChatId, Action, Optionals) -> Resp when
    Token :: token(),
    ChatId :: chat_id(),
    Action :: binary(),
    Optionals :: send_chat_action_optionals(),
    Resp :: tbc_response(true).
send_chat_action(Token, ChatId, Action, Optionals) ->
    Params0 = #{
        <<"chat_id">> => ChatId,
        <<"action">> => Action
    },
    Params = maps:merge(Params0, Optionals),
    send(Token, "sendChatAction", Params). 

-spec get_user_profile_photos(Token, UserId, Optionals) -> Resp when
    Token :: token(),
    UserId :: user_id(),
    Optionals :: get_user_profile_photos_optionals(),
    Resp :: tbc_response(user_profile_photos()).
get_user_profile_photos(Token, UserId, Optionals) ->
    Params0 = #{
        <<"user_id">> => UserId
    },
    Params = maps:merge(Params0, Optionals),
    send(Token, "getUserProfilePhotos", Params).

-spec get_file(Token, FileId) -> Resp when
    Token :: token(),
    FileId :: binary(),
    Resp :: tbc_response(file()).
get_file(Token, FileId) ->
    Params = #{
        <<"file_id">> => FileId
    },
    send(Token, "getFile", Params).

-spec ban_chat_member(Token, ChatId, UserId, Optionals) -> Resp when
    Token :: token(),
    ChatId :: chat_id(),
    UserId :: user_id(),
    Optionals :: ban_chat_member_optionals(),
    Resp :: tbc_response(true).
ban_chat_member(Token, ChatId, UserId, Optionals) ->
    Params0 = #{
        <<"chat_id">> => ChatId,
        <<"user_id">> => UserId
    },
    Params = maps:merge(Params0, Optionals),
    send(Token, "banChatMember", Params).

-spec unban_chat_member(Token, ChatId, UserId, Optionals) -> Resp when
    Token :: token(),
    ChatId :: chat_id(),
    UserId :: user_id(),
    Optionals :: unban_chat_member_optionals(),
    Resp :: tbc_response(true).
unban_chat_member(Token, ChatId, UserId, Optionals) ->
    Params0 = #{
        <<"chat_id">> => ChatId,
        <<"user_id">> => UserId
    },
    Params = maps:merge(Params0, Optionals),
    send(Token, "unbanChatMember", Params).

-spec restrict_chat_member(Token, ChatId, UserId, Permissions, Optionals) -> Resp when
    Token :: token(),
    ChatId :: chat_id(),
    UserId :: user_id(),
    Permissions :: chat_permissions(),
    Optionals :: restrict_chat_member_optionals(),
    Resp :: tbc_response(true).
restrict_chat_member(Token, ChatId, UserId, Permissions, Optionals) ->
    Params0 = #{
        <<"chat_id">> => ChatId,
        <<"user_id">> => UserId,
        <<"permissions">> => Permissions
    },
    Params = maps:merge(Params0, Optionals),
    send(Token, "restrictChatMember", Params).

-spec promote_chat_member(Token, ChatId, UserId, Optionals) -> Resp when
    Token :: token(),
    ChatId :: chat_id(),
    UserId :: user_id(),
    Optionals :: promote_chat_member_optionals(),
    Resp :: tbc_response(true).
promote_chat_member(Token, ChatId, UserId, Optionals) ->
    Params0 = #{
        <<"chat_id">> => ChatId,
        <<"user_id">> => UserId
    },
    Params = maps:merge(Params0, Optionals),
    send(Token, "promoteChatMember", Params).

-spec set_chat_administrator_custom_title(Token, ChatId, UserId, CustomTitle) -> Resp when
    Token :: token(),
    ChatId :: chat_id(),
    UserId :: user_id(),
    CustomTitle :: binary(),
    Resp :: tbc_response(true).
set_chat_administrator_custom_title(Token, ChatId, UserId, CustomTitle) ->
    Params = #{
        <<"chat_id">> => ChatId,
        <<"user_id">> => UserId,
        <<"custom_title">> => CustomTitle
    },
    send(Token, "setChatAdministratorCustomTitle", Params).

-spec ban_chat_sender_chat(Token, ChatId, SenderChatId) -> Resp when
    Token :: token(),
    ChatId :: chat_id(),
    SenderChatId :: integer(),
    Resp :: tbc_response(true).
ban_chat_sender_chat(Token, ChatId, SenderChatId) ->
    Params = #{
        <<"chat_id">> => ChatId,
        <<"sender_chat_id">> => SenderChatId
    },
    send(Token, "promoteChatMember", Params).

-spec unban_chat_sender_chat(Token, ChatId, SenderChatId) -> Resp when
    Token :: token(),
    ChatId :: chat_id(),
    SenderChatId :: integer(),
    Resp :: tbc_response(true).
unban_chat_sender_chat(Token, ChatId, SenderChatId) ->
    Params = #{
        <<"chat_id">> => ChatId,
        <<"sender_chat_id">> => SenderChatId
    },
    send(Token, "unbanChatSenderChat", Params).

-spec set_chat_permissions(Token, ChatId, Permissions, Optionals) -> Resp when
    Token :: token(),
    ChatId :: chat_id(),
    Permissions :: chat_permissions(),
    Optionals :: set_chat_permissions_optionals(),
    Resp :: tbc_response(true).
set_chat_permissions(Token, ChatId, Permissions, Optionals) ->
    Params0 = #{
        <<"chat_id">> => ChatId,
        <<"permissions">> => Permissions
    },
    Params = maps:merge(Params0, Optionals),
    send(Token, "setChatPermissions", Params).

-spec export_chat_invite_link(Token, ChatId) -> Resp when
    Token :: token(),
    ChatId :: chat_id(),
    Resp :: tbc_response(binary()).
export_chat_invite_link(Token, ChatId) ->
    Params = #{
        <<"chat_id">> => ChatId
    },
    send(Token, "exportChatInviteLink", Params).


-spec create_chat_invite_link(Token, ChatId, Optionals) -> Resp when
    Token :: token(),
    ChatId :: chat_id(),
    Optionals :: create_chat_invite_link_optionals(),
    Resp :: tbc_response(chat_invite_link()).
create_chat_invite_link(Token, ChatId, Optionals) ->
    Params0 = #{
        <<"chat_id">> => ChatId
    },
    Params = maps:merge(Params0, Optionals),
    send(Token, "createChatInviteLink", Params).

-spec edit_chat_invite_link(Token, ChatId, InviteLink, Optionals) -> Resp when
    Token :: token(),
    ChatId :: chat_id(),
    InviteLink :: binary(),
    Optionals :: create_chat_invite_link_optionals(),
    Resp :: tbc_response(chat_invite_link()).
edit_chat_invite_link(Token, ChatId, InviteLink, Optionals) ->
    Params0 = #{
        <<"chat_id">> => ChatId,
        <<"invite_link">> => InviteLink
    },
    Params = maps:merge(Params0, Optionals),
    send(Token, "editChatInviteLink", Params).

-spec revoke_chat_invite_link(Token, ChatId, InviteLink) -> Resp when
    Token :: token(),
    ChatId :: chat_id(),
    InviteLink :: binary(),
    Resp :: tbc_response(chat_invite_link()).
revoke_chat_invite_link(Token, ChatId, InviteLink) ->
    Params = #{
        <<"chat_id">> => ChatId,
        <<"invite_link">> => InviteLink
    },
    send(Token, "revokeChatInviteLink", Params).

-spec approve_chat_join_request(Token, ChatId, UserId) -> Resp when
    Token :: token(),
    ChatId :: chat_id(),
    UserId :: user_id(),
    Resp :: tbc_response(true).
approve_chat_join_request(Token, ChatId, UserId) ->
    Params = #{
        <<"chat_id">> => ChatId,
        <<"user_id">> => UserId
    },
    send(Token, "approveChatJoinRequest", Params).

-spec decline_chat_join_request(Token, ChatId, UserId) -> Resp when
    Token :: token(),
    ChatId :: chat_id(),
    UserId :: user_id(),
    Resp :: tbc_response(true).
decline_chat_join_request(Token, ChatId, UserId) ->
    Params = #{
        <<"chat_id">> => ChatId,
        <<"user_id">> => UserId
    },
    send(Token, "declineChatJoinRequest", Params).

-spec set_chat_photo(Token, ChatId, Photo) -> Resp when
    Token :: token(),
    ChatId :: chat_id(),
    Photo :: input_file(),
    Resp :: tbc_response(true).
set_chat_photo(Token, ChatId, Photo) ->
    Params = #{
        <<"chat_id">> => ChatId,
        <<"photo">> => Photo
    },
    send(Token, "setChatPhoto", Params).

-spec delete_chat_photo(Token, ChatId) -> Resp when
    Token :: token(),
    ChatId :: chat_id(),
    Resp :: tbc_response(true).
delete_chat_photo(Token, ChatId) ->
    Params = #{
        <<"chat_id">> => ChatId
    },
    send(Token, "deleteChatPhoto", Params).

-spec set_chat_title(Token, ChatId, Title) -> Resp when
    Token :: token(),
    ChatId :: chat_id(),
    Title :: binary(),
    Resp :: tbc_response(true).
set_chat_title(Token, ChatId, Title) ->
    Params = #{
        <<"chat_id">> => ChatId,
        <<"title">> => Title
    },
    send(Token, "setChatTitle", Params).

-spec set_chat_description(Token, ChatId, Optionals) -> Resp when
    Token :: token(),
    ChatId :: chat_id(),
    Optionals :: set_chat_description_optionals(),
    Resp :: tbc_response(true).
set_chat_description(Token, ChatId, Optionals) ->
    Params0 = #{
        <<"chat_id">> => ChatId
    },
    Params = maps:merge(Params0, Optionals),
    send(Token, "setChatDescription", Params).

-spec pin_chat_message(Token, ChatId, MessageId, Optionals) -> Resp when
    Token :: token(),
    ChatId :: chat_id(),
    MessageId :: integer(),
    Optionals :: pin_chat_message_optionals(),
    Resp :: tbc_response(true).
pin_chat_message(Token, ChatId, MessageId, Optionals) ->
    Params0 = #{
        <<"chat_id">> => ChatId,
        <<"message_id">> => MessageId
    },
    Params = maps:merge(Params0, Optionals),
    send(Token, "pinChatMessage", Params).

-spec unpin_chat_message(Token, ChatId, Optionals) -> Resp when
    Token :: token(),
    ChatId :: chat_id(),
    Optionals :: unpin_chat_message_optionals(),
    Resp :: tbc_response(true).
unpin_chat_message(Token, ChatId, Optionals) ->
    Params0 = #{
        <<"chat_id">> => ChatId
    },
    Params = maps:merge(Params0, Optionals),
    send(Token, "unpinChatMessage", Params).

-spec unpin_all_chat_message(Token, ChatId) -> Resp when
    Token :: token(),
    ChatId :: chat_id(),
    Resp :: tbc_response(true).
unpin_all_chat_message(Token, ChatId) ->
    Params = #{
        <<"chat_id">> => ChatId
    },
    send(Token, "unpinAllChatMessage", Params).

-spec leave_chat(Token, ChatId) -> Resp when
    Token :: token(),
    ChatId :: chat_id(),
    Resp :: tbc_response(true).
leave_chat(Token, ChatId) ->
    Params = #{
        <<"chat_id">> => ChatId
    },
    send(Token, "leaveChat", Params).

-spec get_chat(Token, ChatId) -> Resp when
    Token :: token(),
    ChatId :: chat_id(),
    Resp :: tbc_response(chat()).
get_chat(Token, ChatId) ->
    Params = #{
        <<"chat_id">> => ChatId
    },
    send(Token, "getChat", Params).

-spec get_chat_administrators(Token, ChatId) -> Resp when
    Token :: token(),
    ChatId :: chat_id(),
    Resp :: tbc_response([chat_member()]).
get_chat_administrators(Token, ChatId) ->
    Params = #{
        <<"chat_id">> => ChatId
    },
    send(Token, "getChatAdministrators", Params).

-spec get_chat_member_count(Token, ChatId) -> Resp when
    Token :: token(),
    ChatId :: chat_id(),
    Resp :: tbc_response(integer()).
get_chat_member_count(Token, ChatId) ->
    Params = #{
        <<"chat_id">> => ChatId
    },
    send(Token, "getChatMemberCount", Params).

-spec get_chat_member(Token, ChatId, UserId) -> Resp when
    Token :: token(),
    ChatId :: chat_id(),
    UserId :: user_id(),
    Resp :: tbc_response(chat_member()).
get_chat_member(Token, ChatId, UserId) ->
    Params = #{
        <<"chat_id">> => ChatId,
        <<"user_id">> => UserId
    },
    send(Token, "getChatMember", Params).

-spec set_chat_sticker_set(Token, ChatId, StickerSetName) -> Resp when
    Token :: token(),
    ChatId :: chat_id(),
    StickerSetName :: binary(),
    Resp :: tbc_response(boolean()).
set_chat_sticker_set(Token, ChatId, StickerSetName) ->
    Params = #{
        <<"chat_id">> => ChatId,
        <<"sticker_set_name">> => StickerSetName
    },
    send(Token, "setChatStickerSet", Params).

-spec delete_chat_sticker_set(Token, ChatId) -> Resp when
    Token :: token(),
    ChatId :: chat_id(),
    Resp :: tbc_response(true).
delete_chat_sticker_set(Token, ChatId) ->
    Params = #{
        <<"chat_id">> => ChatId
    },
    send(Token, "deleteChatStickerSet", Params).

-spec get_forum_topic_icon_stickers(Token) -> Resp when
    Token :: token(),
    Resp :: tbc_response([sticker()]).
get_forum_topic_icon_stickers(Token) ->
    send(Token, "getForumTopicIconStickers", #{}).

-spec create_forum_topic(Token, ChatId, Name, Optionals) -> Resp when
    Token :: token(),
    ChatId :: chat_id(),
    Name :: binary(),
    Optionals :: create_forum_topic_optionals(),
    Resp :: tbc_response(forum_topic()).
create_forum_topic(Token, ChatId, Name, Optionals) ->
    Params0 = #{
        <<"chat_id">> => ChatId,
        <<"name">> => Name
    },
    Params = maps:merge(Params0, Optionals),
    send(Token, "createForumTopic", Params).

-spec edit_forum_topic(Token, ChatId, MessageThreadId, Optionals) -> Resp when
    Token :: token(),
    ChatId :: chat_id(),
    MessageThreadId :: integer(),
    Optionals :: edit_forum_topic_optionals(),
    Resp :: tbc_response(true).
edit_forum_topic(Token, ChatId, Name, Optionals) ->
    Params0 = #{
        <<"chat_id">> => ChatId,
        <<"name">> => Name
    },
    Params = maps:merge(Params0, Optionals),
    send(Token, "editForumTopic", Params).

-spec close_forum_topic(Token, ChatId, MessageThreadId) -> Resp when
    Token :: token(),
    ChatId :: chat_id(),
    MessageThreadId :: integer(),
    Resp :: tbc_response(true).
close_forum_topic(Token, ChatId, MessageThreadId) ->
    Params = #{
        <<"chat_id">> => ChatId,
        <<"message_trhead_id">> => MessageThreadId
    },
    send(Token, "closeForumTopic", Params).

-spec reopen_forum_topic(Token, ChatId, MessageThreadId) -> Resp when
    Token :: token(),
    ChatId :: chat_id(),
    MessageThreadId :: integer(),
    Resp :: tbc_response(true).
reopen_forum_topic(Token, ChatId, MessageThreadId) ->
    Params = #{
        <<"chat_id">> => ChatId,
        <<"message_trhead_id">> => MessageThreadId
    },
    send(Token, "reopenForumTopic", Params).

-spec delete_forum_topic(Token, ChatId, MessageThreadId) -> Resp when
    Token :: token(),
    ChatId :: chat_id(),
    MessageThreadId :: integer(),
    Resp :: tbc_response(true).
delete_forum_topic(Token, ChatId, MessageThreadId) ->
    Params = #{
        <<"chat_id">> => ChatId,
        <<"message_trhead_id">> => MessageThreadId
    },
    send(Token, "deleteForumTopic", Params).

-spec unpin_all_forum_topic_messages(Token, ChatId, MessageThreadId) -> Resp when
    Token :: token(),
    ChatId :: chat_id(),
    MessageThreadId :: integer(),
    Resp :: tbc_response(true).
unpin_all_forum_topic_messages(Token, ChatId, MessageThreadId) ->
    Params = #{
        <<"chat_id">> => ChatId,
        <<"message_trhead_id">> => MessageThreadId
    },
    send(Token, "unpinAllForumTopicMessages", Params).

-spec edit_general_forum_topic(Token, ChatId, Name) -> Resp when
    Token :: token(),
    ChatId :: chat_id(),
    Name :: binary(),
    Resp :: tbc_response(true).
edit_general_forum_topic(Token, ChatId, MessageThreadId) ->
    Params = #{
        <<"chat_id">> => ChatId,
        <<"message_trhead_id">> => MessageThreadId
    },
    send(Token, "editGeneralForumTopic", Params).

-spec close_general_forum_topic(Token, ChatId) -> Resp when
    Token :: token(),
    ChatId :: chat_id(),
    Resp :: tbc_response(true).
close_general_forum_topic(Token, ChatId) ->
    Params = #{
        <<"chat_id">> => ChatId
    },
    send(Token, "closeGeneralForumTopic", Params).

-spec reopen_general_forum_topic(Token, ChatId) -> Resp when
    Token :: token(),
    ChatId :: chat_id(),
    Resp :: tbc_response(true).
reopen_general_forum_topic(Token, ChatId) ->
    Params = #{
        <<"chat_id">> => ChatId
    },
    send(Token, "reopenGeneralForumTopic", Params).

-spec hide_general_forum_topic(Token, ChatId) -> Resp when
    Token :: token(),
    ChatId :: chat_id(),
    Resp :: tbc_response(true).
hide_general_forum_topic(Token, ChatId) ->
    Params = #{
        <<"chat_id">> => ChatId
    },
    send(Token, "hideGeneralForumTopic", Params).

-spec unhide_general_forum_topic(Token, ChatId) -> Resp when
    Token :: token(),
    ChatId :: chat_id(),
    Resp :: tbc_response(true).
unhide_general_forum_topic(Token, ChatId) ->
    Params = #{
        <<"chat_id">> => ChatId
    },
    send(Token, "unhideGeneralForumTopic", Params).

-spec answer_callback_query(Token, CallbackQueryId, Optionals) -> Resp when
    Token :: token(),
    CallbackQueryId :: chat_id(),
    Optionals :: answer_callback_query_optionals(),
    Resp :: tbc_response(true).
answer_callback_query(Token, CallbackQueryId, Optionals) ->
    Params0 = #{
        <<"callback_query_id">> => CallbackQueryId
    },
    Params = maps:merge(Params0, Optionals),
    send(Token, "answerCallbackQuery", Params).

-spec set_my_commands(Token, Commands, Optionals) -> Resp when
    Token :: token(),
    Commands :: [bot_command()],
    Optionals :: set_my_commands_optionals(),
    Resp :: tbc_response(true).
set_my_commands(Token, Commands, Optionals) ->
    Params0 = #{
        <<"commands">> => Commands
    },
    Params = maps:merge(Params0, Optionals),
    send(Token, "setMyCommands", Params).

-spec delete_my_commands(Token, Optionals) -> Resp when
    Token :: token(),
    Optionals :: get_my_commands_optionals(),
    Resp :: tbc_response(true).
delete_my_commands(Token, Optionals) ->
    send(Token, "deleteMyCommands", Optionals).

-spec get_my_commands(Token, Optionals) -> Resp when
    Token :: token(),
    Optionals :: get_my_commands_optionals(),
    Resp :: tbc_response([bot_command()]).
get_my_commands(Token, Optionals) ->
    send(Token, "getMyCommands", Optionals).

-spec get_chat_menu_button(Token, Optionals) -> Resp when
    Token :: token(),
    Optionals :: get_chat_menu_button_optionals(),
    Resp :: tbc_response(menu_button()).
get_chat_menu_button(Token, Optionals) ->
    send(Token, "getChatMenuButton", Optionals).

-spec set_my_default_administrator_rights(Token, Optionals) -> Resp when
    Token :: token(),
    Optionals :: set_my_default_administrator_rights_optionals(),
    Resp :: tbc_response(true).
set_my_default_administrator_rights(Token, Optionals) ->
    send(Token, "setMyDefaultAdministratorRights", Optionals).

-spec get_my_default_administrator_rights(Token, Optionals) -> Resp when
    Token :: token(),
    Optionals :: get_my_default_administrator_rights_optionals(),
    Resp :: tbc_response(true).
get_my_default_administrator_rights(Token, Optionals) ->
    send(Token, "getMyDefaultAdministratorRights", Optionals).

-spec get_updates(Token, Optionals) -> Resp when
    Token :: token(),
    Optionals :: get_updates_optionals(),
    Resp :: tbc_response([update()]).
get_updates(Token, Optionals) ->
    send(Token, "getUpdates", Optionals).


%%% ---------------------------------------------
%%% INTERNAL FUNCTIONS
%%% ---------------------------------------------
send(Token, ApiMethod, Params) ->
    HttpMethod = post,
    UriString = "https://api.telegram.org/bot" ++ Token ++ "/" ++ ApiMethod,
    Headers = [],
    ContentType = "application/json",
    Body = njson:encode(Params),
    HttpOptions = [{ssl, [{verify, verify_none}]}],
    Request = {UriString, Headers, ContentType, Body},
    case httpc:request(HttpMethod, Request, HttpOptions, [{body_format, binary}]) of
        {ok, Result} ->
            {ok, handle_result(Result)};
        {error, Reason} = Err ->
            ?LOG_ERROR("[tbc] HTTP call failed. Reason ~p",[Reason]),
            Err
    end.

handle_result({{_HttpVersion, _StatusCode, _ReasonPhrase}, _Headers, BodyResult}) ->
    njson:decode(BodyResult).
