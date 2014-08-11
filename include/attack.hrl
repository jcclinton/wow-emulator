
-define(hitinfo_normalswing, 16#00000000).
-define(hitinfo_unk0, 16#00000001).
-define(hitinfo_normalswing2, 16#00000002).
-define(hitinfo_leftswing, 16#00000004).
-define(hitinfo_unk3, 16#00000008).
-define(hitinfo_miss, 16#00000010).
-define(hitinfo_absorb, 16#00000020).
-define(hitinfo_resist, 16#00000040).
-define(hitinfo_criticalhit, 16#00000080).
-define(hitinfo_unk8, 16#00000100).
-define(hitinfo_unk9, 16#00002000).
-define(hitinfo_glancing, 16#00004000).
-define(hitinfo_crushing, 16#00008000).
-define(hitinfo_noaction, 16#00010000).
-define(hitinfo_swingnohitsound, 16#00080000).

-define(victimstate_unaffected, 0).
-define(victimstate_normal, 1).
-define(victimstate_dodge, 2).
-define(victimstate_parry, 3).
-define(victimstate_interrupt, 4).
-define(victimstate_blocks, 5).
-define(victimstate_evades, 6).
-define(victimstate_is_immune, 7).
-define(victimstate_deflects, 8).

-define(base_meleerange_offset, 2.66).
