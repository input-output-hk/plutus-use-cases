import { toast } from 'react-toastify';
import * as fromApi from '../api/tokenActions';

import { formatSellData, formatBuyData } from '../helpers/utils';
import {
  FETCH_SELL_TOKEN_START,
  FETCH_SELL_TOKEN_SUCCESS,
  FETCH_SELL_TOKEN_FAILED,
  FETCH_BUY_TOKEN_START,
  FETCH_BUY_TOKEN_SUCCESS,
  FETCH_BUY_TOKEN_FAILED,
} from '../helpers/actionTypes';

export const fetchSellTokenStart = () => ({
  type: FETCH_SELL_TOKEN_START,
});

export const fetchSellTokenSuccess = () => ({
  type: FETCH_SELL_TOKEN_SUCCESS,
});

export const fetchSellTokenFailed = (error) => ({
  type: FETCH_SELL_TOKEN_FAILED,
  error,
});

export const fetchBuyTokenStart = () => ({
  type: FETCH_BUY_TOKEN_START,
});

export const fetchBuyTokenSuccess = () => ({
  type: FETCH_BUY_TOKEN_SUCCESS,
});

export const fetchBuyTokenFailed = (error) => ({
  type: FETCH_BUY_TOKEN_FAILED,
  error,
});

export const fetchSellToken = (wallet, data) => async (dispatch) => {
  dispatch(fetchSellTokenStart());
  const response = await fromApi.fetchSellToken(wallet, formatSellData(data));
  if (response.error) {
    dispatch(fetchSellTokenFailed(response.error));
    toast.error(response.error);
  } else {
    dispatch(fetchSellTokenSuccess());
    toast.success('Token has been selled');
  }
};

export const fetchBuyToken = (wallet, tokenId) => async (dispatch) => {
  dispatch(fetchBuyTokenStart());
  const response = await fromApi.fetchBuyToken(wallet, formatBuyData(tokenId));
  if (response.error) {
    dispatch(fetchBuyTokenFailed(response.error));
    toast.error(response.error);
  } else {
    dispatch(fetchBuyTokenSuccess());
    toast.success('Token has been bought');
  }
};
