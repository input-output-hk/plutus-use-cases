import { combineReducers } from 'redux';
import {
  FETCH_GAME_BETS_START,
  FETCH_GAME_BETS_SUCCESS,
  FETCH_GAME_BETS_FAILED,
  FETCH_MAKE_BET_START,
  FETCH_MAKE_BET_SUCCESS,
  FETCH_MAKE_BET_FAILED,
  FETCH_CANCEL_BET_START,
  FETCH_CANCEL_BET_SUCCESS,
  FETCH_CANCEL_BET_FAILED,
  LOGOUT,
} from '../helpers/actionTypes';

export const data = (state = null, action) => {
  switch (action.type) {
    case FETCH_GAME_BETS_SUCCESS:
      return action.bets;
    case FETCH_MAKE_BET_SUCCESS:
      return state ? [action.bet, ...state] : [action.bet];
    case FETCH_CANCEL_BET_SUCCESS:
      return state.filter(
        (bet) =>
          !(
            bet.betBettor.unPaymentPubKeyHash.getPubKeyHash === action.bet.betBettor &&
            bet.betAmount.getLovelace === action.bet.betAmount &&
            bet.betTeamId === action.bet.betTeamId
          )
      );
    case LOGOUT:
      return null;
    default:
      return state;
  }
};

export const fetching = (state = false, action) => {
  switch (action.type) {
    case FETCH_GAME_BETS_START:
    case FETCH_MAKE_BET_START:
    case FETCH_CANCEL_BET_START:
      return true;
    case FETCH_GAME_BETS_SUCCESS:
    case FETCH_GAME_BETS_FAILED:
    case FETCH_MAKE_BET_SUCCESS:
    case FETCH_MAKE_BET_FAILED:
    case FETCH_CANCEL_BET_SUCCESS:
    case FETCH_CANCEL_BET_FAILED:
      return false;
    default:
      return state;
  }
};

export const error = (state = '', action) => {
  switch (action.type) {
    case FETCH_GAME_BETS_SUCCESS:
    case FETCH_MAKE_BET_SUCCESS:
    case FETCH_CANCEL_BET_SUCCESS:
      return '';
    case FETCH_GAME_BETS_FAILED:
    case FETCH_MAKE_BET_FAILED:
    case FETCH_CANCEL_BET_FAILED:
      return action.error;
    default:
      return state;
  }
};

const bets = combineReducers({
  data,
  fetching,
  error,
});

export const getGameBets = (state) => state.data;
export const getGameBetsFetching = (state) => state.fetching;

export default bets;
