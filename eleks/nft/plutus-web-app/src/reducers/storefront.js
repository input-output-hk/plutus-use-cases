import { combineReducers } from 'redux';
import {
  FETCH_STOREFRONT_START,
  FETCH_STOREFRONT_SUCCESS,
  FETCH_STOREFRONT_FAILED,
  LOGOUT_USER
} from '../helpers/actionTypes';

export const data = (state = [], action) => {
  switch (action.type) {
    case FETCH_STOREFRONT_SUCCESS:
      return action.tokens;
    case LOGOUT_USER:
      return [];
    default:
      return state;
  }
};

export const fetching = (state = false, action) => {
  switch (action.type) {
    case FETCH_STOREFRONT_START:
      return true;
    case FETCH_STOREFRONT_SUCCESS:
    case FETCH_STOREFRONT_FAILED:
      return false;
    default:
      return state;
  }
};

export const error = (state = '', action) => {
  switch (action.type) {
    case FETCH_STOREFRONT_SUCCESS:
      return '';
    case FETCH_STOREFRONT_FAILED:
      return action.error;
    default:
      return state;
  }
};

const storefront = combineReducers({
  data,
  fetching,
  error,
});

export const getStorefront = (state) => state.data;
export const getStorefrontFetching = (state) => state.fetching;
export const getStorefrontError = (state) => state.error;

export default storefront;
