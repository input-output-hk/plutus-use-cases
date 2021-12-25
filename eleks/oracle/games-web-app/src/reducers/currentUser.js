import { combineReducers } from 'redux';
import {
  FETCH_LOGIN_START,
  FETCH_LOGIN_SUCCESS,
  FETCH_LOGIN_FAILED,
  LOGOUT,
} from '../helpers/actionTypes';

const intialState = JSON.parse(localStorage.getItem('currentUser')) || null;

export const data = (state = intialState, action) => {
  switch (action.type) {
    case FETCH_LOGIN_SUCCESS:
      return action.wallet;
    case LOGOUT:
      return null;
    default:
      return state;
  }
};

export const fetching = (state = false, action) => {
  switch (action.type) {
    case FETCH_LOGIN_START:
      return true;
    case FETCH_LOGIN_SUCCESS:
    case FETCH_LOGIN_FAILED:
      return false;
    default:
      return state;
  }
};

export const error = (state = '', action) => {
  switch (action.type) {
    case FETCH_LOGIN_SUCCESS:
      return '';
    case FETCH_LOGIN_FAILED:
      return action.error;
    default:
      return state;
  }
};

const currentUser = combineReducers({
  data,
  fetching,
  error,
});

export const getCurrentUser = (state) => state.data;
export const getCurrentUserFetching = (state) => state.fetching;

export default currentUser;
