import { combineReducers } from 'redux';
import {
  FETCH_MY_TOKENS_START,
  FETCH_MY_TOKENS_SUCCESS,
  FETCH_MY_TOKENS_FAILED,
  FETCH_ADD_TOKEN_START,
  FETCH_ADD_TOKEN_SUCCESS,
  FETCH_ADD_TOKEN_FAILED,
} from '../helpers/actionTypes';

export const data = (state = [], action) => {
  switch (action.type) {
    case FETCH_MY_TOKENS_SUCCESS:
      return action.tokens;
    default:
      return state;
  }
};

export const fetching = (state = false, action) => {
  switch (action.type) {
    case FETCH_ADD_TOKEN_START:
    case FETCH_MY_TOKENS_START:
      return true;
    case FETCH_ADD_TOKEN_SUCCESS:
    case FETCH_ADD_TOKEN_FAILED:
    case FETCH_MY_TOKENS_SUCCESS:
    case FETCH_MY_TOKENS_FAILED:
      return false;
    default:
      return state;
  }
};

export const error = (state = '', action) => {
  switch (action.type) {
    case FETCH_ADD_TOKEN_SUCCESS:
    case FETCH_MY_TOKENS_SUCCESS:
      return '';
    case FETCH_ADD_TOKEN_FAILED:
    case FETCH_MY_TOKENS_FAILED:
      return action.error;
    default:
      return state;
  }
};

const myTokens = combineReducers({
  data,
  fetching,
  error
});

export const getMyTokens = (state) => state.data;
export const getMyTokensFetching = (state) => state.fetching;
export const getMyTokensError = (state) => state.error;

export default myTokens;
