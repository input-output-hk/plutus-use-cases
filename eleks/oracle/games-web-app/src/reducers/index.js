import { combineReducers } from 'redux';
import { connectRouter } from 'connected-react-router';

import currentUser, * as fromCurrentUser from './currentUser';
import games, * as fromGames from './games';
import game, * as fromGame from './game';
import bets, * as fromBets from './bets';

export default (history) =>
  combineReducers({
    router: connectRouter(history),
    currentUser,
    games,
    game,
    bets,
  });

export const getCurrentUser = (state) =>
  fromCurrentUser.getCurrentUser(state.currentUser);
export const getCurrentUserFetching = (state) =>
  fromCurrentUser.getCurrentUserFetching(state.currentUser);

export const getGames = (state) => fromGames.getGames(state.games);
export const getGamesFetching = (state) =>
  fromGames.getGamesFetching(state.games);

export const getGame = (state) => fromGame.getGame(state.game);
export const getGameFetching = (state) => fromGame.getGameFetching(state.game);

export const getGameBets = (state) => fromBets.getGameBets(state.bets);
export const getGameBetsFetching = (state) =>
  fromBets.getGameBetsFetching(state.bets);
