// Calls the four response interfaces the way Steam does, through their virtual methods, so that the
// test exercises the real C++ ABI path against the hand built Pascal vtables.
//
// This file is compiled against the actual Steamworks headers, which is what makes the vtable
// expectation on this side genuine rather than a second guess of the Pascal one.
#include "steam_api.h"
#include "isteammatchmaking.h"

extern "C" {

void CallServerListResponse(ISteamMatchmakingServerListResponse *p)
{
  p->ServerResponded((HServerListRequest)0x1111,7);
  p->ServerFailedToRespond((HServerListRequest)0x2222,8);
  p->RefreshComplete((HServerListRequest)0x3333,eServerFailedToRespond);
}

void CallPingResponse(ISteamMatchmakingPingResponse *p,gameserveritem_t *item)
{
  p->ServerResponded(*item);
  p->ServerFailedToRespond();
}

void CallPlayersResponse(ISteamMatchmakingPlayersResponse *p)
{
  p->AddPlayerToList("BeRo",4711,12.5f);
  p->PlayersFailedToRespond();
  p->PlayersRefreshComplete();
}

void CallRulesResponse(ISteamMatchmakingRulesResponse *p)
{
  p->RulesResponded("mapname","de_dust2");
  p->RulesFailedToRespond();
  p->RulesRefreshComplete();
}

}
