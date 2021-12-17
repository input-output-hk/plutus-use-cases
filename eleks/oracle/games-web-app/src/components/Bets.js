import { connect } from 'react-redux';
import { compose, withState, withHandlers } from 'recompose';
import { lovelaceToAda, isGameOpen } from '../helpers/utils';
import { cancelBet } from '../actions/bets';
import { getCurrentUser } from '../reducers';
import { Button, Modal } from 'react-bootstrap';
import { ReactComponent as Cross } from '../icons/cross.svg';

import '../styles/Bets.scss';

const Bets = ({
  bets,
  game,
  currentUser,
  betToCancel,
  setBetToCancel,
  handleCancel,
}) => (
  <>
    <div className='Bets'>
      {bets.map((bet, i) => {
        const isMyBet =
          currentUser && bet.betBettor.unPaymentPubKeyHash.getPubKeyHash === currentUser.publicKey;
        return (
          <div className={`bet ${isMyBet ? 'myBet' : ''}`} key={i}>
            <span className='hash'>{bet.betBettor.unPaymentPubKeyHash.getPubKeyHash}</span>
            <span className='amount'>
              {lovelaceToAda(bet.betAmount.getLovelace)} ADA
            </span>
            {isMyBet && isGameOpen(game) && (
              <div className='cancel' onClick={() => setBetToCancel(bet)}>
                <Cross width='20px' height='20px' />
              </div>
            )}
            {!isMyBet && isGameOpen(game) && (
              <div className='crossPlaceholder' />
            )}
          </div>
        );
      })}
    </div>
    {cancelBet && (
      <Modal
        show={betToCancel}
        onHide={() => setBetToCancel(false)}
        size='sm'
        aria-labelledby='contained-modal-title-vcenter'
        centered
        className='CancelBet'
      >
        <Modal.Header closeButton>
          <Modal.Title id='contained-modal-title-vcenter'>
            Cancel bet
          </Modal.Title>
        </Modal.Header>
        <Modal.Body className='body'>
          <p>
            Are you sure you want to cancel a bet? This action is irreversible.
          </p>
        </Modal.Body>
        <Modal.Footer className='footer'>
          <Button onClick={handleCancel} variant='secondary' type='submit'>
            Cancel a bet
          </Button>
          <Button onClick={() => setBetToCancel(false)} className='btn-danger'>
            Quit
          </Button>
        </Modal.Footer>
      </Modal>
    )}
  </>
);

const enhancer = compose(
  withState('betToCancel', 'setBetToCancel', false),
  connect(
    (state) => ({
      wallet: getCurrentUser(state),
    }),
    (dispatch) => ({
      cancelBet: (team, amount, gameId, wallet) =>
        dispatch(cancelBet(team, amount, gameId, wallet)),
    })
  ),
  withHandlers({
    handleCancel: ({
      betToCancel,
      cancelBet,
      setBetToCancel,
      game,
      wallet,
    }) => (ev) => {
      cancelBet(
        betToCancel.betTeamId,
        betToCancel.betAmount.getLovelace,
        game.contractId,
        wallet
      );
      setBetToCancel(false);
    },
  })
);

export default enhancer(Bets);
