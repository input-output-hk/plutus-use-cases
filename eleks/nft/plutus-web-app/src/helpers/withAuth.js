import { connect } from 'react-redux';
import { push } from 'connected-react-router';
import { compose, withHandlers } from 'recompose';

import { getCurrentUser } from '../reducers/currentUser';
import { loginUser, logoutUser } from '../actions/currentUser';

export const withAuth = compose(
  connect(
    (state) => ({
      currentUser: getCurrentUser(state),
    }),
    (dispatch) => ({
      redirect: (path) => dispatch(push(path)),
      loginUser: (wallet) => dispatch(loginUser(wallet)),
      logoutUser: () => dispatch(loginUser(logoutUser)),
    })
  ),
  withHandlers({
    login: ({ loginUser, redirect }) => (wallet) => {
      localStorage.setItem('currentUser', JSON.stringify(wallet));
      loginUser(wallet);
      redirect('/');
    },
    logout: ({ logoutUser, redirect }) => () => {
      localStorage.removeItem('currentUser');
      logoutUser();
      redirect('/login');
    },
  })
);
