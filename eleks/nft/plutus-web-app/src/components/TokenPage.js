import { connect } from 'react-redux';
import { compose, withState, withProps } from 'recompose';

import { getTokenActionsFetching } from '../reducers';
import { fetchSellToken } from '../actions/tokenActions';

import Loader from './Loader';
import SellModal from './SellModal';
import Coin from '../icons/coin.gif';
import Button from 'react-bootstrap/Button';

import '../styles/TokenPage.scss';

const TokenPage = ({
  token,
  showModal,
  setShowModal,
  fetchSellToken,
  tokenActionFetching,
}) => (
  <div className='TokenPage'>
    <img
      className='image'
      src={token.image ? `https://ipfs.io/ipfs/${token.image}` : Coin}
    />
    <h5>{token.name}</h5>
    {!token.price && (
      <Button variant='secondary' onClick={() => setShowModal(true)}>
        Sell token
      </Button>
    )}
    <SellModal
      show={showModal}
      setShowModal={setShowModal}
      token={token}
      fetchSellToken={fetchSellToken}
    />
    {tokenActionFetching && (
      <Loader
        disableBackground={true}
        text={'Please, be patient. Token is selling...'}
      />
    )}
  </div>
);

const enhancer = compose(
  withState('showModal', 'setShowModal', false),
  withProps(() => ({
    token: JSON.parse(localStorage.getItem('viewSingleToken')),
  })),
  connect(
    (state) => ({
      tokenActionFetching: getTokenActionsFetching(state),
    }),
    (dispatch, props) => ({
      fetchSellToken: (data) =>
        dispatch(fetchSellToken(props.currentUser, data)),
    })
  )
);

export default enhancer(TokenPage);
