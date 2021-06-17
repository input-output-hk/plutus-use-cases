import { toast } from 'react-toastify';
import * as fromApi from '../api/tokenActions';

import {
  formatSellData,
  formatObjectResponse,
  formatBuyResponse,
} from '../helpers/utils';
import {
  SET_TOKEN,
  FETCH_SELL_TOKEN_START,
  FETCH_SELL_TOKEN_SUCCESS,
  FETCH_SELL_TOKEN_FAILED,
  FETCH_BUY_TOKEN_START,
  FETCH_BUY_TOKEN_SUCCESS,
  FETCH_BUY_TOKEN_FAILED,
  FETCH_CANCEL_SELL_TOKEN_START,
  FETCH_CANCEL_SELL_TOKEN_SUCCESS,
  FETCH_CANCEL_SELL_TOKEN_FAILED,
  FETCH_TRANSFER_TOKEN_START,
  FETCH_TRANSFER_TOKEN_SUCCESS,
  FETCH_TRANSFER_TOKEN_FAILED,
} from '../helpers/actionTypes';

export const setToken = (token) => ({
  type: SET_TOKEN,
  token
});

export const fetchSellTokenStart = () => ({
  type: FETCH_SELL_TOKEN_START,
});

export const fetchSellTokenSuccess = (token) => ({
  type: FETCH_SELL_TOKEN_SUCCESS,
  token,
});

export const fetchSellTokenFailed = (error) => ({
  type: FETCH_SELL_TOKEN_FAILED,
  error,
});

export const fetchBuyTokenStart = () => ({
  type: FETCH_BUY_TOKEN_START,
});

export const fetchBuyTokenSuccess = (token) => ({
  type: FETCH_BUY_TOKEN_SUCCESS,
  token,
});

export const fetchBuyTokenFailed = (error) => ({
  type: FETCH_BUY_TOKEN_FAILED,
  error,
});

export const fetchCancelSellStart = () => ({
  type: FETCH_CANCEL_SELL_TOKEN_START,
});

export const fetchCancelSellSuccess = (token) => ({
  type: FETCH_CANCEL_SELL_TOKEN_SUCCESS,
  token,
});

export const fetchCancelSellFailed = (error) => ({
  type: FETCH_CANCEL_SELL_TOKEN_FAILED,
  error,
});

export const fetchTransferStart = () => ({
  type: FETCH_TRANSFER_TOKEN_START,
});

export const fetchTransferSuccess = (token) => ({
  type: FETCH_TRANSFER_TOKEN_SUCCESS,
  token,
});

export const fetchTransferFailed = (error) => ({
  type: FETCH_TRANSFER_TOKEN_FAILED,
  error,
});

export const fetchSellToken = (wallet, data) => async (dispatch) => {
  dispatch(fetchSellTokenStart());
  const response = await fromApi.fetchSellToken(wallet, formatSellData(data));
  if (response.error) {
    dispatch(fetchSellTokenFailed(response.error));
    toast.error(response.error);
  } else {
    const token = formatObjectResponse(response);
    localStorage.setItem('viewSingleToken', JSON.stringify(token));
    dispatch(fetchSellTokenSuccess(token));
    toast.success('Token has been put on sale');
  }
};

export const fetchBuyToken = (wallet, tokenId) => async (dispatch) => {
  dispatch(fetchBuyTokenStart());
  const response = await fromApi.fetchBuyToken(wallet, tokenId);
  if (response.error) {
    dispatch(fetchBuyTokenFailed(response.error));
    toast.error(response.error);
  } else {
    const token = formatBuyResponse(response);
    localStorage.setItem('viewSingleToken', JSON.stringify(token));
    dispatch(fetchBuyTokenSuccess(token));
    toast.success('Token has been bought');
  }
};

export const fetchCancelSellToken = (wallet, tokenId) => async (dispatch) => {
  dispatch(fetchCancelSellStart());
  const response = await fromApi.fetchCancelSellToken(wallet, tokenId);
  if (response.error) {
    dispatch(fetchCancelSellFailed(response.error));
    toast.error(response.error);
  } else {
    const token = formatBuyResponse(response);
    localStorage.setItem('viewSingleToken', JSON.stringify(token));
    dispatch(fetchCancelSellSuccess(token));
    toast.success('Token sell has been canceled');
  }
};

export const fetchTransferToken = (wallet, data) => async (dispatch) => {
  dispatch(fetchTransferStart());
  const response = await fromApi.fetchTransferToken(wallet, data);
  if (response.error) {
    dispatch(fetchTransferFailed(response.error));
    toast.error(response.error);
  } else {
    dispatch(
      fetchTransferSuccess({
        ...formatObjectResponse(response),
        transfered: true,
      })
    );
    toast.success('Token has been transfered');
  }
};
