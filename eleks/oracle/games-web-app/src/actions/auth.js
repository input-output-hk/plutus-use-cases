import { LOGIN, LOGOUT } from '../helpers/actionTypes';

export const login = (wallet) => ({
  type: LOGIN,
  wallet,
});

export const logout = () => ({
  type: LOGOUT,
});
