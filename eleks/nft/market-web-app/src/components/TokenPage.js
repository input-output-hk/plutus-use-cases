import React from 'react';
import { connect } from 'react-redux';
import { compose, withProps, withState } from 'recompose';

import { getToken, getTokenActionsFetching } from '../reducers';
import {
  fetchSellToken,
  fetchBuyToken,
  fetchCancelSellToken,
  fetchTransferToken,
} from '../actions/tokenActions';

import Loader from './Loader';
import SellModal from './SellModal';
import Coin from '../icons/coin.gif';
import Card from 'react-bootstrap/Card';
import ConfirmModal from './ConfirmModal';
import TransferModal from './TransferModal';
import Button from 'react-bootstrap/Button';
import ListGroup from 'react-bootstrap/ListGroup';
import ListGroupItem from 'react-bootstrap/ListGroupItem';

import '../styles/TokenPage.scss';

const TokenPage = ({
  token,
  showModal,
  setShowModal,
  fetchBuyToken,
  fetchSellToken,
  fetchCancelSellToken,
  fetchTransferToken,
  tokenActionFetching,
  currentUser,
  isBuy,
}) => (
  <div className='TokenPage'>
    <h3 className='heading'>Token view page</h3>
    <div className='card-container'>
      <Card.Img
        src={token.image ? `https://ipfs.io/ipfs/${token.image}` : Coin}
      />
      <Card>
        <Card.Body>
          <Card.Title>{token.name}</Card.Title>
          <Card.Text>{token.description}</Card.Text>
        </Card.Body>

        <Card.Body>
          <ListGroup className='list-group-flush'>
            <ListGroupItem>
              <Card.Subtitle className='mb-2 text-muted'>Author:</Card.Subtitle>
              {token.author || 'No author'}
            </ListGroupItem>
            <ListGroupItem>
              <Card.Subtitle className='mb-2 text-muted'>
                Seller id:
              </Card.Subtitle>
              {token.seller || 'No seller'}
            </ListGroupItem>
            <ListGroupItem>
              <Card.Subtitle className='mb-2 text-muted'>Price:</Card.Subtitle>
              {token.price ? `${token.price} ADA` : 'Token is not selling'}
            </ListGroupItem>
          </ListGroup>
        </Card.Body>
        {!token.transfered && (
          <React.Fragment>
            {!token.price && (
              <Card.Body>
                <Button
                  variant='secondary'
                  onClick={() => setShowModal('sell')}
                >
                  Sell token
                </Button>
                <Button
                  variant='secondary'
                  onClick={() => setShowModal('transfer')}
                  className='margin-left'
                >
                  Transfer token
                </Button>
              </Card.Body>
            )}

            {!!token.price && currentUser.publicKey !== token.seller && (
              <Card.Body>
                <Button variant='secondary' onClick={() => setShowModal('buy')}>
                  Buy token
                </Button>
              </Card.Body>
            )}

            {!!token.price && currentUser.publicKey === token.seller && (
              <Card.Body>
                <Button
                  variant='secondary'
                  onClick={() => setShowModal('cancel')}
                >
                  Cancel sell
                </Button>
              </Card.Body>
            )}
          </React.Fragment>
        )}
      </Card>
    </div>

    <SellModal
      show={showModal === 'sell'}
      setShowModal={setShowModal}
      token={token}
      fetchSellToken={fetchSellToken}
    />

    <TransferModal
      show={showModal === 'transfer'}
      setShowModal={setShowModal}
      token={token}
      fetchTransferToken={fetchTransferToken}
      currentUser={currentUser}
    />

    <ConfirmModal
      show={isBuy || showModal === 'cancel'}
      setShowModal={setShowModal}
      token={token}
      fetchTokenAction={isBuy ? fetchBuyToken : fetchCancelSellToken}
      header={isBuy ? 'Buy token' : 'Cancel sell'}
      text={
        isBuy
          ? `Do you want to buy ${token.name} token for ${token.price} ADA?`
          : `Do you want to cancel sell ${token.name} token?`
      }
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
  withProps(({ showModal }) => ({ isBuy: showModal === 'buy' })),
  connect(
    (state) => ({
      token: getToken(state),
      tokenActionFetching: getTokenActionsFetching(state),
    }),
    (dispatch, props) => ({
      fetchSellToken: (data) =>
        dispatch(fetchSellToken(props.currentUser, data)),
      fetchBuyToken: (data) => dispatch(fetchBuyToken(props.currentUser, data)),
      fetchCancelSellToken: (data) =>
        dispatch(fetchCancelSellToken(props.currentUser, data)),
      fetchTransferToken: (data) =>
        dispatch(fetchTransferToken(props.currentUser, data)),
    })
  )
);

export default enhancer(TokenPage);
