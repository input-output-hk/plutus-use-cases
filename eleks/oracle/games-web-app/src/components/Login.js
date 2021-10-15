import { compose, withHandlers, withState, withProps } from 'recompose';
import { Redirect } from 'react-router-dom';
import { connect } from 'react-redux';
import Select from 'react-select';
import Form from 'react-bootstrap/Form';
import Button from 'react-bootstrap/Button';
import Loader from './Loader';
import { login } from '../actions/auth';
import { getCurrentUser, getCurrentUserFetching } from '../reducers';
import { wallets } from '../helpers/constants';

import '../styles/Login.scss';

const Login = ({
  onSubmit,
  options,
  setWallet,
  currentUser,
  errorVisibility,
  currentUserFetching,
}) =>
  currentUser ? (
    <Redirect to='/' />
  ) : (
    <div className='Login'>
      <Form onSubmit={onSubmit} className='form'>
        <h2 className='heading'>Login</h2>
        <h6 className='subheading'>
          Choose wallet and start your <br /> investigation of Mutual Betting
          Platform
        </h6>
        <Form.Group controlId='formWalletId'>
          <Select
            options={options}
            onChange={(el) => setWallet(el)}
            placeholder='Select your wallet'
          />
        </Form.Group>
        <Form.Text className={`error-text ${errorVisibility && 'visible'}`}>
          Please select your wallet to login the system
        </Form.Text>
        <Button className='button' variant='secondary' type='submit'>
          Login
        </Button>
      </Form>
      {currentUserFetching && (
        <Loader disableBackground={true} text={'Login into the system...'} />
      )}
    </div>
  );

const enhancer = compose(
  withState('wallet', 'setWallet'),
  withState('error', 'setError'),
  withProps(({ error, wallet }) => ({
    options: wallets.map((wallet) => ({
      label: `Wallet ${wallet}`,
      value: wallet,
    })),
    errorVisibility: error && !wallet,
  })),
  connect(
    (state) => ({
      currentUser: getCurrentUser(state),
      currentUserFetching: getCurrentUserFetching(state),
    }),
    (dispatch) => ({
      login: (wallet) => dispatch(login(wallet)),
    })
  ),
  withHandlers({
    onSubmit: ({ wallet, login, setError }) => (ev) => {
      ev.preventDefault();
      if (!wallet) {
        setError(true);
      } else {
        setError(false);
        login(wallet);
      }
    },
  })
);

export default enhancer(Login);
