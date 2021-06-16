import { compose, withState, withHandlers } from 'recompose';

import Modal from 'react-bootstrap/Modal';
import Button from 'react-bootstrap/Button';
import FormControl from 'react-bootstrap/FormControl';

import '../styles/SellModal.scss';

const SellModal = ({ show, onHide, price, setPrice, submitSell, error }) => (
  <Modal
    show={show}
    onHide={onHide}
    size='sm'
    aria-labelledby='contained-modal-title-vcenter'
    centered
    className='SellModal'
  >
    <Modal.Header closeButton>
      <Modal.Title id='contained-modal-title-vcenter'>Sell token</Modal.Title>
    </Modal.Header>
    <Modal.Body>
      <p>Fill the price in ADA to sell token</p>
      <FormControl
        placeholder='1000'
        aria-label='Price'
        aria-describedby='basic-addon1'
        value={price}
        onChange={(ev) => setPrice(ev.target.value)}
        className={error && 'error'}
      />
      <span className={`text-error ${error && 'visible'}`}>
        {error || 'Error'}
      </span>
    </Modal.Body>
    <Modal.Footer>
      <Button onClick={submitSell} variant='secondary' type='submit'>
        Sell token
      </Button>
      <Button onClick={onHide} className='btn-danger'>
        Close
      </Button>
    </Modal.Footer>
  </Modal>
);

const enhancer = compose(
  withState('price', 'setPrice', ''),
  withState('error', 'setError', ''),
  withHandlers({
    onHide: ({ setShowModal, setError }) => (ev) => {
      setShowModal(false);
      setError(false);
    },
    submitSell: ({
      token,
      price,
      setError,
      fetchSellToken,
      setShowModal,
      setPrice,
    }) => () => {
      var reg = new RegExp('^((-)?(0|([1-9][0-9]*))(.[0-9]+)?)$');
      if (!price) {
        setError('Price should be filled');
      } else if (price <= 0) {
        setError('Price should be larger than zero');
      } else if (!reg.test(price)) {
        setError('Price should be a number');
      } else {
        setShowModal(false);
        setError(false);
        setPrice('');
        fetchSellToken({ id: token.id, price });
      }
    },
  })
);

export default enhancer(SellModal);
