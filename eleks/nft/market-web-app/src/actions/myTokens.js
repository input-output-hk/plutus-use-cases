import { toast } from 'react-toastify';

import * as fromApi from '../api/myTokens';
import { formatResponse, formatObjectResponse } from '../helpers/utils';
import {
  FETCH_MY_TOKENS_START,
  FETCH_MY_TOKENS_SUCCESS,
  FETCH_MY_TOKENS_FAILED,
  FETCH_ADD_TOKEN_START,
  FETCH_ADD_TOKEN_SUCCESS,
  FETCH_ADD_TOKEN_FAILED,
} from '../helpers/actionTypes';

export const fetchMyTokensStart = () => ({
  type: FETCH_MY_TOKENS_START,
});

export const fetchMyTokensSuccess = (tokens) => ({
  type: FETCH_MY_TOKENS_SUCCESS,
  tokens,
});

export const fetchMyTokensFailed = (error) => ({
  type: FETCH_MY_TOKENS_FAILED,
  error,
});

export const fetchAddTokenStart = () => ({
  type: FETCH_ADD_TOKEN_START,
});

export const fetchAddTokenSuccess = (token) => ({
  type: FETCH_ADD_TOKEN_SUCCESS,
  token,
});

export const fetchAddTokenFailed = (error) => ({
  type: FETCH_ADD_TOKEN_FAILED,
  error,
});

export const fetchMyTokens = (wallet) => async (dispatch) => {
  dispatch(fetchMyTokensStart());
  const tokens = await fromApi.fetchMyTokens(wallet);
  if (tokens.error) {
    dispatch(fetchMyTokensFailed(tokens.error));
    toast.error(tokens.error);
  } else {
    dispatch(fetchMyTokensSuccess(formatResponse(tokens)));
  }
};

export const fetchAddToken = (wallet, data) => async (dispatch) => {
  dispatch(fetchAddTokenStart());
  const token = await fromApi.fetchAddToken(wallet, data);
  if (token.error) {
    dispatch(fetchAddTokenFailed(token.error));
    toast.error(token.error);
  } else {
    dispatch(fetchAddTokenSuccess(formatObjectResponse(token)));
    toast.success('Token has been created');
  }
};
