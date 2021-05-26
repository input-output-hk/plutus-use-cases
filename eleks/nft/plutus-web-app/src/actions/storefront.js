import * as fromApi from '../api/storefront';
import { formatResponse } from '../helpers/utils';
import {
  FETCH_STOREFRONT_START,
  FETCH_STOREFRONT_SUCCESS,
  FETCH_STOREFRONT_FAILED,
} from '../helpers/actionTypes';

export const fetchStorefrontStart = () => ({
  type: FETCH_STOREFRONT_START,
});

export const fetchStorefrontSuccess = (tokens) => ({
  type: FETCH_STOREFRONT_SUCCESS,
  tokens,
});

export const fetchStorefrontFailed = (error) => ({
  type: FETCH_STOREFRONT_FAILED,
  error,
});

export const fetchStorefront = (wallet) => async (dispatch) => {
  dispatch(fetchStorefrontStart());
  const tokens = await fromApi.fetchStorefront(wallet);
  tokens.error
    ? dispatch(fetchStorefrontFailed(tokens.error))
    : dispatch(fetchStorefrontSuccess(formatResponse(tokens)));
};