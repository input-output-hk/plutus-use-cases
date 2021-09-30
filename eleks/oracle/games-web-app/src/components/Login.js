import { compose, withHandlers, withState, withProps } from 'recompose';
import { Redirect } from 'react-router-dom';
import { connect } from 'react-redux';
import Select from 'react-select';
import Form from 'react-bootstrap/Form';
import Button from 'react-bootstrap/Button';
import { login } from '../actions/auth';
import { getCurrentUser } from '../reducers';

import '../styles/Login.scss';

const Login = ({ onSubmit, options, setWallet, currentUser }) => {
  return currentUser ? (
    <Redirect to='/' />
  ) : (
    <div className='Login'>
      <Form onSubmit={onSubmit} className='form'>
        <Form.Group controlId='formWalletId'>
          <Select
            options={options}
            onChange={(el) => setWallet(el)}
            placeholder='Select your wallet'
          />
        </Form.Group>
        <Button className='button' variant='secondary' type='submit'>
          Login
        </Button>
      </Form>
    </div>
  );
};

const enhancer = compose(
  withState('wallet', 'setWallet'),
  withProps(() => ({
    options: [
      //todo
      { label: 'Wallet 1', value: 1 },
      { label: 'Wallet 2', value: 2 },
      { label: 'Wallet 3', value: 3 },
      { label: 'Wallet 4', value: 4 },
    ],
  })),
  connect(
    (state) => ({
      currentUser: getCurrentUser(state),
    }),
    (dispatch) => ({
      login: (wallet) => dispatch(login(wallet)),
    })
  ),
  withHandlers({
    onSubmit: ({ wallet, login }) => (ev) => {
      ev.preventDefault();
      if (wallet) {
        localStorage.setItem('currentUser', JSON.stringify(wallet));
        login(wallet);
      }
    },
  })
);

export default enhancer(Login);
