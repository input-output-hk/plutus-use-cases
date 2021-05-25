import { LOGIN_USER, LOGOUT_USER } from '../helpers/actionTypes';

const intialState = JSON.parse(localStorage.getItem('currentUser'));

export const currentUser = (state = intialState, action) => {
  switch (action.type) {
    case LOGIN_USER:
      return { ...action.user };
    case LOGOUT_USER:
      return { ...action.user };
    default:
      return state;
  }
};

export const getCurrentUser = (state) => state.currentUser;

export default currentUser;
