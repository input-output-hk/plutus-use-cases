import { LOGIN_USER, LOGOUT_USER } from '../helpers/actionTypes';

export const loginUser = (user) => ({
  type: LOGIN_USER,
  user,
});

export const logoutUser = () => ({
  type: LOGOUT_USER,
});
