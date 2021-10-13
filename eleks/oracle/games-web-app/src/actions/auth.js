import { toast } from 'react-toastify';
import * as fromApi from '../api/auth';
import {
  FETCH_LOGIN_START,
  FETCH_LOGIN_SUCCESS,
  FETCH_LOGIN_FAILED,
  LOGOUT,
} from '../helpers/actionTypes';

export const fetchLoginStart = () => ({
  type: FETCH_LOGIN_START,
});

export const fetchLoginSuccess = (wallet) => ({
  type: FETCH_LOGIN_SUCCESS,
  wallet,
});

export const fetchLoginFailed = (error) => ({
  type: FETCH_LOGIN_FAILED,
  error,
});

export const logout = () => ({
  type: LOGOUT,
});

export const login = (wallet) => async (dispatch) => {
  dispatch(fetchLoginStart());
  const walletKey = await fromApi.login(wallet.value);
  if (walletKey.error) {
    dispatch(fetchLoginFailed(walletKey.error));
    toast.error(walletKey.error);
  } else {
    const user = {
      ...wallet,
      walletId: walletKey.walletId,
      publicKey: walletKey.walletDataPubKeyHash.getPubKeyHash,
    };
    localStorage.setItem('currentUser', JSON.stringify(user));
    dispatch(fetchLoginSuccess(user));
  }
};
