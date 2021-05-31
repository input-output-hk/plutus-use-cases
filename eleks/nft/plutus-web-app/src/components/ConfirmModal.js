import { compose, withHandlers } from 'recompose';

import Modal from 'react-bootstrap/Modal';
import Button from 'react-bootstrap/Button';

import '../styles/ConfirmModal.scss';

const ConfirmModal = ({ show, setShowModal, submitSell, header, text }) => (
  <Modal
    show={show}
    onHide={() => setShowModal(false)}
    size='sm'
    aria-labelledby='contained-modal-title-vcenter'
    centered
    className='ConfirmModal'
  >
    <Modal.Header closeButton>
      <Modal.Title id='contained-modal-title-vcenter'>{header}</Modal.Title>
    </Modal.Header>
    <Modal.Body>
      <p>{text}</p>
    </Modal.Body>
    <Modal.Footer>
      <Button onClick={submitSell} variant='secondary' type='submit'>
        {header}
      </Button>
      <Button onClick={() => setShowModal(false)} className='btn-danger'>
        Cancel
      </Button>
    </Modal.Footer>
  </Modal>
);

const enhancer = compose(
  withHandlers({
    submitSell: ({ token, fetchTokenAction, setShowModal }) => () => {
      setShowModal(false);
      fetchTokenAction({ id: token.id });
    },
  })
);

export default enhancer(ConfirmModal);
