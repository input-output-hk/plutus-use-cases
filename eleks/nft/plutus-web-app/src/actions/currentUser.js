import { toast } from 'react-toastify';

import * as fromApi from '../api/currentUser';
import { formatKeyResponse } from '../helpers/utils';
import {
  LOGOUT,
  FETCH_LOGIN_START,
  FETCH_LOGIN_SUCCESS,
  FETCH_LOGIN_FAILED,
} from '../helpers/actionTypes';

export const logoutUser = () => ({
  type: LOGOUT,
});

export const fetchUserPublicKeyStart = () => ({
  type: FETCH_LOGIN_START,
});

export const fetchUserPublicKeySuccess = (user) => ({
  type: FETCH_LOGIN_SUCCESS,
  user,
});

export const fetchUserPublicKeyFailed = (error) => ({
  type: FETCH_LOGIN_FAILED,
  error,
});

export const fetchLoginUser = (wallet) => async (dispatch) => {
  dispatch(fetchUserPublicKeyStart());
  const response = await fromApi.fetchUserPublicKey(wallet);
  if (response.error) {
    dispatch(fetchUserPublicKeyFailed(response.error));
    toast.error(response.error);
  } else {
    const user = {
      ...wallet,
      publicKey: response.contents,
    };
    dispatch(fetchUserPublicKeySuccess(user));
    localStorage.setItem('currentUser', JSON.stringify(user));
  }
};
