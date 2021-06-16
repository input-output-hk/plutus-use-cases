import { combineReducers } from 'redux';
import {
  FETCH_STOREFRONT_START,
  FETCH_STOREFRONT_SUCCESS,
  FETCH_STOREFRONT_FAILED,
  FETCH_SELL_TOKEN_SUCCESS,
  FETCH_BUY_TOKEN_SUCCESS,
  FETCH_CANCEL_SELL_TOKEN_SUCCESS,
  LOGOUT,
} from '../helpers/actionTypes';

export const data = (state = [], action) => {
  switch (action.type) {
    case FETCH_STOREFRONT_SUCCESS:
      return action.tokens;
    case FETCH_SELL_TOKEN_SUCCESS:
    case FETCH_BUY_TOKEN_SUCCESS:
    case FETCH_CANCEL_SELL_TOKEN_SUCCESS:
    case LOGOUT:
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
