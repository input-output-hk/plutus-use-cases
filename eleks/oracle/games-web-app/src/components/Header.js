import { compose, withHandlers } from 'recompose';
import { connect } from 'react-redux';
import { logout } from '../actions/auth';
import { Link } from 'react-router-dom';
import Button from 'react-bootstrap/Button';
import '../styles/Header.scss';
import { getCurrentUser } from '../reducers';

const Header = ({ handleLogout, currentUser }) => {
  return (
    <div className='Header'>
      <Link className='link' to='/'>
        <h2 className='title'>Mutual Betting Platform</h2>
      </Link>
      {currentUser && (
        <div className='rightPart'>
          <span>Signed in as: {currentUser.label}</span>
          <Button
            className='logoutBtn'
            variant='secondary'
            onClick={handleLogout}
          >
            Logout
          </Button>
        </div>
      )}
    </div>
  );
};

const enhancer = compose(
  connect(
    (state) => ({
      currentUser: getCurrentUser(state),
    }),
    (dispatch) => ({
      logout: () => dispatch(logout()),
    })
  ),
  withHandlers({
    handleLogout: ({ logout }) => (ev) => {
      localStorage.removeItem('currentUser');
      logout();
    },
  })
);

export default enhancer(Header);
