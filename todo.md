#### erlang specific

* move processing out of servers and into process pools
    * world broadcasting, player_character updates, callback worker

* move data into mnesia
    * currently it is using ets/dets
    * also ets caching needs to be made more efficient, right now it stores everything in memory



#### game features

* stats/item modifications
* melee combat
* spell system
