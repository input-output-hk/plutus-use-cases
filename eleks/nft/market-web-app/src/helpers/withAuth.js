import { connect } from 'react-redux';
import { push } from 'connected-react-router';
import { compose, withHandlers } from 'recompose';

import { fetchLoginUser, logoutUser } from '../actions/currentUser';
import { getCurrentUser, getCurrentUserFetching } from '../reducers';

export const withAuth = compose(
  connect(
    (state) => ({
      currentUser: getCurrentUser(state),
      currentUserFetching: getCurrentUserFetching(state),
    }),
    (dispatch) => ({
      redirect: (path) => dispatch(push(path)),
      fetchLoginUser: (wallet) => dispatch(fetchLoginUser(wallet)),
      logoutUser: () => dispatch(logoutUser()),
    })
  ),
  withHandlers({
    login: ({ fetchLoginUser }) => (wallet) => {
      fetchLoginUser(wallet);
    },
    logout: ({ logoutUser, redirect }) => () => {
      localStorage.removeItem('currentUser');
      logoutUser();
      redirect('/login');
    },
  })
);
