import { combineReducers } from 'redux';
import {
  FETCH_GAMES_START,
  FETCH_GAMES_SUCCESS,
  FETCH_GAMES_FAILED,
  LOGOUT,
} from '../helpers/actionTypes';

export const data = (state = [], action) => {
  switch (action.type) {
    case FETCH_GAMES_SUCCESS:
      return action.games;
    case LOGOUT:
      return [];
    default:
      return state;
  }
};

export const fetching = (state = false, action) => {
  switch (action.type) {
    case FETCH_GAMES_START:
      return true;
    case FETCH_GAMES_SUCCESS:
    case FETCH_GAMES_FAILED:
      return false;
    default:
      return state;
  }
};

export const error = (state = '', action) => {
  switch (action.type) {
    case FETCH_GAMES_SUCCESS:
      return '';
    case FETCH_GAMES_FAILED:
      return action.error;
    default:
      return state;
  }
};

const games = combineReducers({
  data,
  fetching,
  error,
});

export const getGames = (state) => state.data;
export const getGamesFetching = (state) => state.fetching;

export default games;
