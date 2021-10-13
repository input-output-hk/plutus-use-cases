import { compose, withProps, withHandlers, withState } from 'recompose';
import { connect } from 'react-redux';
import { Modal, Button } from 'react-bootstrap';
import Select from 'react-select';
import { makeBet } from '../actions/bets';
import { getCurrentUser } from '../reducers';
import '../styles/MakeBet.scss';

const customTheme = (theme) => ({
  ...theme,
  colors: {
    ...theme.colors,
    primary50: '#cccccc',
    primary25: '#cccccc',
    primary: '#808080',
  },
});

const MakeBet = ({
  showModal,
  onClose,
  handleSubmit,
  teams,
  setTeam,
  amount,
  setAmount,
  error,
}) => (
  <Modal
    show={showModal}
    onHide={onClose}
    size='sm'
    aria-labelledby='contained-modal-title-vcenter'
    centered
    className='MakeBet'
  >
    <Modal.Header closeButton>
      <Modal.Title id='contained-modal-title-vcenter'>
        Make your bet
      </Modal.Title>
    </Modal.Header>
    <Modal.Body className='body'>
      <p>Please, enter the amount in ADA, that you want to bet.</p>
      <Select
        options={teams}
        onChange={(el) => setTeam(el.value)}
        placeholder={'Team'}
        theme={customTheme}
      />
      <input
        className={'input'}
        value={amount}
        onChange={(ev) => setAmount(ev.target.value)}
        placeholder={'Amount'}
      />
      <span className={`error-text ${error ? 'visible' : ''}`}>
        {error || 'error'}
      </span>
    </Modal.Body>
    <Modal.Footer className='footer'>
      <Button onClick={handleSubmit} variant='secondary' type='submit'>
        Make a bet
      </Button>
      <Button onClick={onClose} className='btn-danger'>
        Cancel
      </Button>
    </Modal.Footer>
  </Modal>
);

const enhancer = compose(
  withState('team', 'setTeam'),
  withState('amount', 'setAmount', ''),
  withState('error', 'setError', ''),
  withProps(({ game }) => {
    const home = game.teams.home;
    const away = game.teams.away;
    return {
      teams: [
        { value: home.id, label: home.name },
        { value: away.id, label: away.name },
      ],
    };
  }),
  connect(
    (state) => ({
      wallet: getCurrentUser(state),
    }),
    (dispatch) => ({
      makeBet: (team, amount, gameId, wallet) =>
        dispatch(makeBet(team, amount, gameId, wallet)),
    })
  ),
  withHandlers({
    onClose: ({ setError, setAmount, setShowModal }) => (ev) => {
      setError(null);
      setAmount('');
      setShowModal(false);
    },
  }),
  withHandlers({
    handleSubmit: ({
      team,
      amount,
      makeBet,
      game,
      setError,
      onClose,
      wallet,
    }) => (ev) => {
      const reg = new RegExp('^[0-9].*$');
      const isNum = reg.test(amount);

      if (!team || !amount) {
        setError('All fields should be filled');
      } else if (!isNum || amount < game.minBet) {
        setError(
          `Amount should be a number and ${'can`t'} be less than ${
            game.minBet
          } ADA`
        );
      } else {
        makeBet(team, amount * 1000000, game.contractId, wallet);
        onClose();
      }
    },
  })
);

export default enhancer(MakeBet);
