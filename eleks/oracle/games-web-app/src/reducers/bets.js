import { combineReducers } from 'redux';
import {
  FETCH_GAME_BETS_START,
  FETCH_GAME_BETS_SUCCESS,
  FETCH_GAME_BETS_FAILED,
} from '../helpers/actionTypes';

export const data = (state = null, action) => {
  switch (action.type) {
    case FETCH_GAME_BETS_SUCCESS:
      return action.bets;
    default:
      return state;
  }
};

export const fetching = (state = false, action) => {
  switch (action.type) {
    case FETCH_GAME_BETS_START:
      return true;
    case FETCH_GAME_BETS_SUCCESS:
    case FETCH_GAME_BETS_FAILED:
      return false;
    default:
      return state;
  }
};

export const error = (state = '', action) => {
  switch (action.type) {
    case FETCH_GAME_BETS_SUCCESS:
      return '';
    case FETCH_GAME_BETS_FAILED:
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

export default bets;

