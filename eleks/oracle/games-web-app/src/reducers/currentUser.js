import { LOGIN, LOGOUT } from '../helpers/actionTypes';

const intialState = JSON.parse(localStorage.getItem('currentUser')) || null;

export const currentUser = (state = intialState, action) => {
  switch (action.type) {
    case LOGIN:
      return action.wallet;
    case LOGOUT:
      return null;
    default:
      return state;
  }
};

export const getCurrentUser = (state) => state;

export default currentUser;
