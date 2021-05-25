import { combineReducers } from 'redux';
import { connectRouter } from 'connected-react-router';

import currentUser, * as fromCurrentUser from './currentUser';
import myTokens, * as fromMyTokens from './myTokens';
import tokenActions, * as fromTokenActions from './tokenActions';

export const getCurrentUser = (state) =>
  fromCurrentUser.getCurrentUser(state.currentUser);

export const getMyTokens = (state) => fromMyTokens.getMyTokens(state.myTokens);
export const getMyTokensFetching = (state) =>
  fromMyTokens.getMyTokensFetching(state.myTokens);
export const getMyTokensError = (state) =>
  fromMyTokens.getMyTokensError(state.myTokens);

export const getTokenActionsFetching = (state) =>
  fromTokenActions.getTokenActionsFetching(state.tokenActions);
export const getTokenActionsError = (state) =>
  fromTokenActions.getTokenActionsError(state.tokenActions);

export default (history) =>
  combineReducers({
    router: connectRouter(history),
    currentUser,
    myTokens,
    tokenActions,
  });
