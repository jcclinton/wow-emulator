-define(wow_success, 16#00).
-define(wow_fail_unknown0, 16#01).                 %< ? Unable to connect
-define(wow_fail_unknown1, 16#02).                 %< ? Unable to connect
-define(wow_fail_banned, 16#03).                 %< This <game> account has been closed and is no longer available for use. Please go to <site>/banned.html for further information.
-define(wow_fail_unknown_account, 16#04).                 %< The information you have entered is not valid. Please check the spelling of the account name and password. If you need help in retrieving a lost or stolen password, see <site> for more information
-define(wow_fail_incorrect_password, 16#05).                 %< The information you have entered is not valid. Please check the spelling of the account name and password. If you need help in retrieving a lost or stolen password, see <site> for more information
    % client reject next login attempts after this error, so in code used WOW_FAIL_UNKNOWN_ACCOUNT for both cases
-define(wow_fail_already_online, 16#06).                 %< This account is already logged into <game>. Please check the spelling and try again.
-define(wow_fail_no_time, 16#07).                 %< You have used up your prepaid time for this account. Please purchase more to continue playing
-define(wow_fail_db_busy, 16#08).                 %< Could not log in to <game> at this time. Please try again later.
-define(wow_fail_version_invalid, 16#09).                 %< Unable to validate game version. This may be caused by file corruption or interference of another program. Please visit <site> for more information and possible solutions to this issue.
-define(wow_fail_version_update, 16#0A).                 %< Downloading
-define(wow_fail_invalid_server, 16#0B).                 %< Unable to connect
-define(wow_fail_suspended, 16#0C).                 %< This <game> account has been temporarily suspended. Please go to <site>/banned.html for further information
-define(wow_fail_fail_noaccess, 16#0D).                 %< Unable to connect
-define(wow_success_survey, 16#0E).                 %< Connected.
-define(wow_fail_parentcontrol, 16#0F).                 %< Access to this account has been blocked by parental controls. Your settings may be changed in your account preferences at <site>
-define(wow_fail_locked_enforced, 16#10).                 %< You have applied a lock to your account. You can change your locked status by calling your account lock phone number.
-define(wow_fail_trial_ended, 16#11).                 %< Your trial subscription has expired. Please visit <site> to upgrade your account.
-define(wow_fail_use_battlenet, 16#12).



-define(cmd_auth_logon_challenge, 16#00).
-define(cmd_auth_logon_proof, 16#01).
-define(cmd_auth_reconnect_challenge, 16#02).
-define(cmd_auth_reconnect_proof, 16#03).
-define(cmd_realm_list, 16#10).
-define(cmd_xfer_initiate, 16#30).
-define(cmd_xfer_data, 16#31).
    % these opcodes no longer exist in currently supported client
-define(cmd_xfer_accept, 16#32).
-define(cmd_xfer_resume, 16#33).
-define(cmd_xfer_cancel, 16#34).
