import { compose, withState, withHandlers, withProps } from 'recompose';

import Select from 'react-select';
import Modal from 'react-bootstrap/Modal';
import Button from 'react-bootstrap/Button';

import '../styles/TransferModal.scss';

const customStyles = {
  control: (provided, state) => ({
    ...provided,
    borderColor: state.selectProps.error ? '#c87373' : '#cccccc',
  }),
};

const TransferModal = ({
  show,
  onHide,
  setWallet,
  options,
  submitTransfer,
  errorVisibility,
  error,
}) => (
  <Modal
    show={show}
    onHide={onHide}
    size='sm'
    aria-labelledby='contained-modal-title-vcenter'
    centered
    className='TransferModal'
  >
    <Modal.Header closeButton>
      <Modal.Title id='contained-modal-title-vcenter'>
        Transfer token
      </Modal.Title>
    </Modal.Header>
    <Modal.Body>
      <p>Choose the wallet of the recipient</p>
      <Select
        options={options}
        onChange={(ev) => setWallet(ev.value)}
        placeholder='Select recipient wallet'
        styles={customStyles}
        error={errorVisibility}
      />
      <span className={`text-error ${errorVisibility && 'visible'}`}>
        {error || 'Error'}
      </span>
    </Modal.Body>
    <Modal.Footer>
      <Button onClick={submitTransfer} variant='secondary' type='submit'>
        Transfer token
      </Button>
      <Button onClick={onHide} className='btn-danger'>
        Close
      </Button>
    </Modal.Footer>
  </Modal>
);

const enhancer = compose(
  withState('wallet', 'setWallet', ''),
  withState('error', 'setError', ''),
  withProps(({ currentUser, wallet, error }) => ({
    options: JSON.parse(process.env.REACT_APP_WALLETS)
      .filter((wallet) => wallet.id !== currentUser.id)
      .map((wallet) => ({ value: wallet, label: wallet.name })),
    errorVisibility: error && !wallet,
  })),
  withHandlers({
    onHide: ({ setShowModal, setError }) => (ev) => {
      setShowModal(false);
      setError(false);
    },
    submitTransfer: ({
      token,
      wallet,
      setError,
      fetchTransferToken,
      setShowModal,
    }) => () => {
      if (!wallet) {
        setError('Wallet should be chosen');
      } else {
        setShowModal(false);
        setError(false);
        fetchTransferToken({ tokenId: token.id, walletNumber: wallet.number });
      }
    },
  })
);

export default enhancer(TransferModal);
