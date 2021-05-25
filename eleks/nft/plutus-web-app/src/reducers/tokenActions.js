import { combineReducers } from 'redux';
import {
  FETCH_SELL_TOKEN_START,
  FETCH_SELL_TOKEN_SUCCESS,
  FETCH_SELL_TOKEN_FAILED,
} from '../helpers/actionTypes';

export const fetching = (state = false, action) => {
  switch (action.type) {
    case FETCH_SELL_TOKEN_START:
      return true;
    case FETCH_SELL_TOKEN_SUCCESS:
    case FETCH_SELL_TOKEN_FAILED:
      return false;
    default:
      return state;
  }
};

export const error = (state = '', action) => {
  switch (action.type) {
    case FETCH_SELL_TOKEN_SUCCESS:
      return '';
    case FETCH_SELL_TOKEN_FAILED:
      return action.error;
    default:
      return state;
  }
};

const tokenActions = combineReducers({
  fetching,
  error,
});

export const getTokenActionsFetching = (state) => state.fetching;
export const getTokenActionsError = (state) => state.error;

export default tokenActions;
