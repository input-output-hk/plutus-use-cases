import { combineReducers } from 'redux';
import {
  FETCH_GAME_START,
  FETCH_GAME_SUCCESS,
  FETCH_GAME_FAILED,
  FETCH_GAME_CONTRACT_SUCCESS,
  LOGOUT,
} from '../helpers/actionTypes';

export const data = (state = null, action) => {
  switch (action.type) {
    case FETCH_GAME_SUCCESS:
      return action.game;
    case FETCH_GAME_CONTRACT_SUCCESS:
      return {
        ...state,
        ...action.contract,
      };
    case LOGOUT:
      return null;
    default:
      return state;
  }
};

export const fetching = (state = false, action) => {
  switch (action.type) {
    case FETCH_GAME_START:
      return true;
    case FETCH_GAME_SUCCESS:
    case FETCH_GAME_FAILED:
      return false;
    default:
      return state;
  }
};

export const error = (state = '', action) => {
  switch (action.type) {
    case FETCH_GAME_SUCCESS:
      return '';
    case FETCH_GAME_FAILED:
      return action.error;
    default:
      return state;
  }
};

const game = combineReducers({
  data,
  fetching,
  error,
});

export const getGame = (state) => state.data;
export const getGameFetching = (state) => state.fetching;

export default game;
